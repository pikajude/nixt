use self::{dependency_queue::DependencyQueue, logger::Logger, queue::Queue};
use crate::{archive::PathFilter, prelude::*};
use crossbeam::thread::Scope;
use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use std::{
  collections::{BTreeSet, HashMap, HashSet},
  io::BufReader,
  os::unix::prelude::*,
  process::*,
  sync::Arc,
};
use tee_readwrite::TeeWriter;
use unix::fcntl::OFlag;

mod dependency_queue;
mod logger;
mod queue;

#[cfg(target_os = "linux")]
#[path = "linux.rs"]
mod sys;

#[cfg(target_os = "macos")]
#[path = "macos.rs"]
mod sys;

#[derive(Debug)]
struct Build {
  child: Child,
  progress: ProgressBar,
}

#[derive(Debug, Deref)]
struct FinishedChild(u32);

#[derive(Debug)]
enum Message {
  Finish {
    job_id: usize,
    outputs: BTreeSet<String>,
    // If `Err`, the build failed and other builds should be killed. If `Ok`, optionally returns
    // the pid of a process that should be removed from self.active_pids.
    result: Result<Option<FinishedChild>>,
  },
  SpawnedProcess(u32),
}

#[derive(Debug)]
pub struct Worker<'a, S: Store = crate::store::LocalStore> {
  queue: DependencyQueue<StorePath, String, Derivation>,
  pending: Vec<(StorePath, Derivation)>,
  active: HashMap<usize, StorePath>,
  messages: Arc<Queue<Message>>,
  next_id: usize,
  active_pids: HashSet<u32>,
  progress: Arc<MultiProgress>,
  store: &'a S,
}

impl<'a, S: Store> Worker<'a, S> {
  pub fn with_store(store: &'a S) -> Self {
    Self {
      store,
      pending: Default::default(),
      queue: Default::default(),
      active: Default::default(),
      messages: Arc::new(Queue::new(100)),
      next_id: 0,
      active_pids: HashSet::new(),
      progress: Arc::new(MultiProgress::new()),
    }
  }

  pub fn add_needed(&mut self, path: &StorePath) -> Result<()> {
    let drv = Derivation::get(&*self.store, path)?;
    let deps_iter = drv
      .input_derivations
      .iter()
      .flat_map(|(path, outputs)| outputs.iter().map(move |o| (path.clone(), o.clone())))
      .collect::<Vec<(StorePath, String)>>();
    for path in drv.input_derivations.keys() {
      if !self.queue.dep_map.contains_key(path) {
        self.add_needed(path)?;
      }
    }
    self.queue.enqueue(path.clone(), drv, deps_iter);
    Ok(())
  }

  fn spawn_if_possible(&mut self, scope: &Scope<'a>) -> Result<()> {
    while let Some((path, drv)) = self.queue.dequeue() {
      self.pending.push((path, drv));
    }

    while !self.pending.is_empty() && self.has_slots() {
      let (path, drv) = self.pending.remove(0);
      self.run(path, drv, scope)?;
    }

    Ok(())
  }

  fn has_slots(&self) -> bool {
    self.active.len() < settings().max_build_jobs
  }

  fn wait_for_events(&mut self) -> Vec<Message> {
    let mut events = self.messages.try_pop_all();
    if events.is_empty() {
      loop {
        match self.messages.pop(Duration::from_millis(500)) {
          Some(message) => {
            events.push(message);
            break;
          }
          None => {
            trace!("waiting for events");
            continue;
          }
        }
      }
    }
    events
  }

  fn drain(&mut self, scope: &Scope<'a>, all_jobs: ProgressBar) -> Result<()> {
    let mut error = None;

    loop {
      if error.is_none() {
        if let Err(e) = self.spawn_if_possible(scope) {
          self.handle_error(&mut error, e, &all_jobs);
        }
      }

      if self.active.is_empty() {
        break;
      }

      for event in self.wait_for_events() {
        if let Err(e) = self.handle_event(event, &all_jobs) {
          self.handle_error(&mut error, e, &all_jobs);
        }
      }
    }

    if let Some(e) = error {
      return Err(e);
    } else if self.queue.is_empty() && self.pending.is_empty() {
      all_jobs.finish_and_clear()
    } else {
      all_jobs.abandon_with_message("internal error: some jobs left in queue")
    }

    Ok(())
  }

  fn handle_error(
    &self,
    some_error: &mut Option<anyhow::Error>,
    error: anyhow::Error,
    all_jobs: &ProgressBar,
  ) {
    if some_error.is_some() {
      warn!("{:?}", error);
    } else {
      if !self.active.is_empty() {
        warn!("{:?}", error);
        all_jobs.println("build failed, waiting for others to finish");
      }
      *some_error = Some(error);
    }
  }

  fn handle_event(&mut self, event: Message, all_jobs: &ProgressBar) -> Result<()> {
    match event {
      Message::Finish {
        job_id,
        outputs,
        result,
      } => {
        let thingy = self.active.remove(&job_id).unwrap();
        for out in &outputs {
          self.queue.finish(&thingy, out);
        }
        match result {
          Ok(x) => {
            debug!("finished {}:{:?}", thingy, outputs);
            all_jobs.inc(1);
            if let Some(pid) = x {
              self.active_pids.remove(&*pid);
            }
          }
          Err(e) => {
            let _ = self.try_show_log(&thingy);
            bail!(e)
          }
        }
      }
      Message::SpawnedProcess(pid) => {
        assert!(self.active_pids.insert(pid));
      }
    }
    Ok(())
  }

  fn try_show_log(&self, failed_path: &StorePath) -> Result<()> {
    let log_path = self.store.logfile_of(failed_path);
    let mut log_lines = Vec::with_capacity(20);
    for l in BufReader::new(fs::File::open(log_path)?).lines() {
      if log_lines.len() == 20 {
        log_lines.remove(0);
      }
      log_lines.push(l?);
    }

    for line in &log_lines {
      info!("{}", line);
    }

    Ok(())
  }

  pub fn build(mut self) -> Result<()> {
    self.queue.queue_finished();

    trace!("{:#?}", self.queue);

    // force the multi-bar to stay alive until all builds are finished
    let all_jobs = self.progress.add(
      ProgressBar::new(self.queue.len() as _).with_style(
        ProgressStyle::default_bar()
          .template("[{bar:40}] {percent}% {pos}/{len}")
          .progress_chars("=> "),
      ),
    );

    let p2 = Arc::clone(&self.progress);
    crate::logger::Logger::set(all_jobs.clone());

    crossbeam::thread::scope(move |scope| {
      scope.spawn(move |_| p2.join_and_clear());

      let result = self.drain(scope, all_jobs);
      crate::logger::Logger::reset();
      result
    })
    .expect("child thread shouldn't panic")
  }

  fn run(&mut self, path: StorePath, drv: Derivation, scope: &Scope<'a>) -> Result<()> {
    let id = self.next_id;
    self.next_id += 1;
    assert!(self.active.insert(id, path.clone()).is_none());

    debug!("attempting to build {}", path);

    let messages = Arc::clone(&self.messages);
    let pog = Arc::clone(&self.progress);
    let store = self.store;

    let doit = move |scope: &Scope<'_>| {
      let mut result = Ok(None);

      let mut needs_build = false;
      for out in drv.outputs.values() {
        if !store.is_valid_path(&out.path).unwrap_or(false) {
          needs_build = true;
          break;
        }
      }

      if !needs_build {
        messages.push(Message::Finish {
          job_id: id,
          outputs: drv.outputs.keys().cloned().collect(),
          result,
        });
        return;
      }

      result = if drv.is_builtin() {
        exec_builtin(store, &messages, &drv, &pog).map(|_| None)
      } else {
        self::sys::exec_builder(store, &messages, scope, &path, &drv, &pog)
      };

      messages.push(Message::Finish {
        job_id: id,
        outputs: drv.outputs.keys().cloned().collect(),
        result,
      });
    };

    scope.spawn(doit);

    Ok(())
  }
}

fn exec_builtin<S: Store>(
  store: &S,
  _messages: &Arc<Queue<Message>>,
  drv: &Derivation,
  progress: &Arc<MultiProgress>,
) -> Result<()> {
  let progress = progress.insert(
    0,
    ProgressBar::new(1).with_style(
      ProgressStyle::default_bar()
        .template("[{elapsed_precise}] {prefix:.green} {bar:40} {bytes}/{total_bytes}"),
    ),
  );
  progress.set_prefix(&drv.name);

  if drv.builder.to_str() == Some("builtin:fetchurl") {
    crate::fetch::fetchurl(&drv, &progress)?;

    store.register_valid_path(ValidPathInfo::new(
      drv.outputs["out"].path.clone(),
      Hash::hash_str("foobar", HashType::SHA256),
    ))?;

    progress.finish_and_clear();

    Ok(())
  } else {
    bail!("unknown builtin: {}", drv.builder.display())
  }
}
