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
use unix::{fcntl::OFlag, sys::signal::Signal};

mod dependency_queue;
mod logger;
mod queue;

#[derive(Debug)]
struct Build {
  child: Child,
  progress: ProgressBar,
}

#[derive(Debug)]
enum Message {
  Finish {
    job_id: usize,
    outputs: BTreeSet<String>,
    result: Result<Option<u32>>,
  },
  SpawnedProcess(u32),
}

#[derive(Debug)]
pub struct Worker {
  queue: DependencyQueue<StorePath, String, Derivation>,
  pending: Vec<(StorePath, Derivation)>,
  active: HashMap<usize, StorePath>,
  messages: Arc<Queue<Message>>,
  next_id: usize,
  active_pids: HashSet<u32>,
  progress: Arc<MultiProgress>,
  store: Arc<dyn Store>,
}

impl Worker {
  pub fn with_store(store: Arc<dyn Store>) -> Self {
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

  fn spawn_if_possible(&mut self, scope: &Scope) -> Result<()> {
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

  fn wait_for_events(&mut self, all_jobs: &ProgressBar) -> Vec<Message> {
    let mut events = self.messages.try_pop_all();
    if events.is_empty() {
      loop {
        all_jobs.tick();
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

  fn drain(&mut self, scope: &Scope, all_jobs: ProgressBar) -> Result<()> {
    let mut error = None;

    loop {
      if error.is_none() {
        if let Err(e) = self.spawn_if_possible(scope) {
          error = Some(e);
          break;
        }
      }

      if self.active.is_empty() {
        break;
      }

      for event in self.wait_for_events(&all_jobs) {
        if let Err(e) = self.handle_event(event, &all_jobs) {
          error = Some(e);
          break;
        }
      }
    }

    if let Some(e) = error {
      // try killing other active jobs
      for pid in &self.active_pids {
        let _ = unix::sys::signal::kill(
          unix::unistd::Pid::from_raw(*pid as _),
          Some(Signal::SIGKILL),
        );
      }
      bail!(e)
    } else if self.queue.is_empty() && self.pending.is_empty() {
      all_jobs.finish_and_clear()
    } else {
      all_jobs.abandon_with_message("internal error: some jobs left in queue")
    }

    Ok(())
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
              self.active_pids.remove(&pid);
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

  fn run(&mut self, path: StorePath, drv: Derivation, scope: &Scope<'_>) -> Result<()> {
    let id = self.next_id;
    self.next_id += 1;
    assert!(self.active.insert(id, path.clone()).is_none());

    debug!("attempting to build {}", path);

    let messages = Arc::clone(&self.messages);
    let pog = Arc::clone(&self.progress);
    let store = Arc::clone(&self.store);

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

      let progress = pog.insert(
        0,
        ProgressBar::new_spinner().with_style(
          ProgressStyle::default_spinner()
            .template("[{elapsed_precise}] {prefix:.green} {wide_msg}"),
        ),
      );
      progress.set_prefix(&drv.name);

      result = if drv.is_builtin() {
        exec_builtin(store, &messages, &drv, progress.clone()).map(|_| None)
      } else {
        exec_builder(store, &messages, scope, &path, &drv, progress.clone())
      };

      let _ = RunOnDrop::new(move || {
        messages.push(Message::Finish {
          job_id: id,
          outputs: drv.outputs.keys().cloned().collect(),
          result,
        });
      });

      progress.finish_and_clear();
    };

    scope.spawn(doit);

    Ok(())
  }
}

fn exec_builtin(
  store: Arc<dyn Store>,
  _messages: &Arc<Queue<Message>>,
  drv: &Derivation,
  progress: ProgressBar,
) -> Result<()> {
  if drv.builder.to_str() == Some("builtin:fetchurl") {
    progress.set_message("fetching...");

    crate::fetch::fetchurl(&drv)?;

    progress.set_message("registering outputs");

    store.register_valid_path(ValidPathInfo::new(
      drv.outputs["out"].path.clone(),
      Hash::hash_str("foobar", HashType::SHA256),
    ))
  } else {
    bail!("unknown builtin: {}", drv.builder.display())
  }
}

fn exec_builder(
  store: Arc<dyn Store>,
  messages: &Arc<Queue<Message>>,
  scope: &Scope<'_>,
  path: &StorePath,
  drv: &Derivation,
  progress: ProgressBar,
) -> Result<Option<u32>> {
  let mut rewrites = HashMap::new();

  for (name, output) in &drv.outputs {
    let out_path = store.print_store_path(&output.path);
    rm_rf(&out_path)?;

    rewrites.insert(crate::derivation::hash_placeholder(name), out_path);
  }

  let mut cmd = Command::new(drv.builder.as_os_str());
  cmd.arg0(&drv.args[0]);
  cmd.args(&drv.args[1..]);
  cmd.env_clear();

  for (ekey, eval) in &drv.env {
    let mut eval_ = eval.to_owned();
    for (find, replace) in &rewrites {
      eval_ = eval_.replace(find, replace);
    }
    cmd.env(ekey, eval_);
  }

  let builder_tmp = tempfile::Builder::new()
    .prefix(format!("nix-build-{}-", drv.name).as_str())
    .tempdir()?;
  cmd.current_dir(&builder_tmp);

  cmd.env("PATH", "/path-not-set");
  cmd.env("HOME", "/homeless-shelter");
  cmd.env("NIX_STORE", store.store_path());
  cmd.env("NIX_BUILD_CORES", settings().build_cores.to_string());
  cmd.env("NIX_LOG_FD", "2");
  cmd.env("TERM", "xterm-256color");

  for alias in &["NIX_BUILD_TOP", "TMPDIR", "TEMPDIR", "TMP", "TEMP", "PWD"] {
    cmd.env(alias, builder_tmp.as_ref().display().to_string());
  }

  let build_log_path = store.logfile_of(path);

  std::fs::create_dir_all(build_log_path.parent().unwrap())?;

  let (pipe_read, pipe_write) = unix::unistd::pipe2(OFlag::O_CLOEXEC)?;

  cmd.stdin(Stdio::null());
  unsafe {
    cmd.stdout(Stdio::from_raw_fd(pipe_write));
    cmd.stderr(Stdio::from_raw_fd(pipe_write));
  }

  let p2 = progress.clone();
  scope.spawn(move |_| Logger::new(build_log_path, pipe_read, p2)?.run());

  let mut child = cmd.spawn()?;
  let pid = child.id();

  messages.push(Message::SpawnedProcess(pid));

  let stat = child.wait()?;
  if !stat.success() {
    bail!(
      "builder for {} failed: {}",
      store.print_store_path(path),
      stat
    );
  }

  progress.set_message("registering outputs");

  // Register the build outputs.
  let mut referenceable_paths = HashSet::<StorePath>::new();

  for out in drv.outputs.values() {
    referenceable_paths.insert(out.path.clone());
  }

  for output in drv.outputs.values() {
    let out_path = store.print_store_path(&output.path);

    canonicalise_path_metadata(&out_path, None)?;

    let mut path_hash = crate::hash::Sink::new(HashType::SHA256);
    let mut scanner = crate::archive::RefsScanner::new(referenceable_paths.iter().map(|p| p.hash));

    debug!("dumping {}", out_path);

    crate::archive::dump_path(
      &out_path,
      TeeWriter::new(&mut path_hash, &mut scanner),
      &PathFilter::none(),
    )?;

    let (path_hash, _) = path_hash.finish();

    debug!("output has hash {}", path_hash.encode(Encoding::Base32));

    store.register_valid_path(ValidPathInfo::new(output.path.clone(), path_hash))?;
  }

  Ok(Some(pid))
}
