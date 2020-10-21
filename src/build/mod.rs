use self::{dependency_queue::DependencyQueue, queue::Queue};
use crate::prelude::*;
use crossbeam::thread::Scope;
use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use serde::Deserialize;
use std::{
  collections::{BTreeSet, HashMap},
  fs::File,
  io::{BufReader, BufWriter},
  os::unix::prelude::*,
  process::*,
  sync::Arc,
};
use unix::fcntl::OFlag;

mod dependency_queue;
mod queue;

#[derive(Debug)]
struct Build {
  child: Child,
  progress: ProgressBar,
}

#[derive(Debug)]
enum Message {
  Finish(usize, BTreeSet<String>, Result<()>),
}

#[derive(Debug)]
pub struct Worker {
  queue: DependencyQueue<StorePath, String, Derivation>,
  pending: Vec<(StorePath, Derivation)>,
  active: HashMap<usize, StorePath>,
  messages: Arc<Queue<Message>>,
  next_id: usize,
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
    self.active.len() < 8
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
            // eprintln!("waiting for events");
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
          eprintln!("{:?}", error);
          break;
        }
      }

      if self.active.is_empty() {
        break;
      }

      for event in self.wait_for_events(&all_jobs) {
        self.handle_event(event, &all_jobs)?;
      }
    }

    if self.queue.is_empty() && self.pending.is_empty() {
      all_jobs.finish_and_clear();
    } else {
      all_jobs.abandon_with_message("internal error: some jobs left in queue");
    }

    Ok(())
  }

  fn handle_event(&mut self, event: Message, all_jobs: &ProgressBar) -> Result<()> {
    match event {
      Message::Finish(job_id, outputs, result) => {
        let thingy = self.active.remove(&job_id).unwrap();
        for out in &outputs {
          self.queue.finish(&thingy, out);
        }
        if result.is_ok() {
          all_jobs.println(format!("finished {}:{:?}", thingy, outputs));
          all_jobs.inc(1);
        } else {
          let _ = self.try_show_log(&thingy, &all_jobs);
          bail!(result.unwrap_err())
        }
      }
    }
    Ok(())
  }

  fn try_show_log(&self, failed_path: &StorePath, all_jobs: &ProgressBar) -> Result<()> {
    let log_path = self.store.logfile_of(failed_path);
    let mut log_lines = Vec::with_capacity(20);
    for l in BufReader::new(std::fs::File::open(log_path)?).lines() {
      if log_lines.len() == 20 {
        log_lines.remove(0);
      }
      log_lines.push(l?);
    }

    for line in &log_lines {
      all_jobs.println(line);
    }

    Ok(())
  }

  pub fn build(mut self) -> Result<()> {
    self.queue.queue_finished();

    // force the multi-bar to stay alive until all builds are finished
    let all_jobs = self.progress.add(
      ProgressBar::new(self.queue.len() as _).with_style(
        ProgressStyle::default_bar()
          .template("[{bar:40}] {percent}% {pos}/{len}")
          .progress_chars("=> "),
      ),
    );

    let p2 = Arc::clone(&self.progress);

    crossbeam::thread::scope(move |scope| {
      scope.spawn(move |_| p2.join_and_clear());

      self.drain(scope, all_jobs)
    })
    .expect("child thread shouldn't panic")
  }

  fn run(&mut self, path: StorePath, drv: Derivation, scope: &Scope<'_>) -> Result<()> {
    let id = self.next_id;
    self.next_id += 1;
    assert!(self.active.insert(id, path.clone()).is_none());

    // eprintln!("attempting to build: {:?}", path);

    let messages = Arc::clone(&self.messages);
    let pog = Arc::clone(&self.progress);
    let store = Arc::clone(&self.store);

    let doit = move |scope: &Scope<'_>| {
      let progress = pog.insert(
        0,
        ProgressBar::new_spinner().with_style(
          ProgressStyle::default_spinner().template("[{elapsed}] {prefix:.green} {wide_msg}"),
        ),
      );
      progress.set_prefix(&drv.name);
      progress.enable_steady_tick(500);

      let result = if drv.is_builtin() {
        if drv.builder.to_str() == Some("builtin:fetchurl") {
          progress.set_message("fetching...");
          crate::fetch::fetchurl(&drv)
        } else {
          Err(anyhow!("unknown builtin: {}", drv.builder.display()))
        }
      } else {
        exec_builder(store, scope, &path, &drv, progress.clone())
      };

      messages.push(Message::Finish(
        id,
        drv.outputs.keys().cloned().collect(),
        result,
      ));

      progress.finish_and_clear();
    };

    scope.spawn(doit);

    Ok(())
  }
}

struct Logger {
  pipe: RawFd,
  file: BufWriter<File>,
  progress: ProgressBar,
  current_line: String,
  phase: Option<String>,
}

impl Logger {
  fn new<P: AsRef<Path>>(path: P, pipe: RawFd, progress: ProgressBar) -> Result<Self> {
    Ok(Self {
      pipe,
      file: BufWriter::new(File::create(path)?),
      progress,
      current_line: String::new(),
      phase: None,
    })
  }

  fn handle_line(&mut self) {
    #[derive(Deserialize)]
    #[serde(rename_all = "camelCase", tag = "action")]
    enum LogMsg {
      Start {
        #[serde(rename = "type")]
        _type: String,
      },
      SetPhase {
        phase: String,
      },
    }

    let l = &self.current_line;
    if l.starts_with("@nix ") {
      if let Ok(msg) = serde_json::from_str::<LogMsg>(&l[5..]) {
        #[allow(clippy::single_match)]
        match msg {
          LogMsg::SetPhase { phase } => {
            self.phase = Some(phase);
          }
          _ => {}
        }
      } else {
        self
          .progress
          .println(&format!("bad JSON message from builder: {:?}", l));
      }
    } else if let Some(ref p) = self.phase {
      self.progress.set_message(&format!("[{}] {}", p, l));
    } else {
      self.progress.set_message(l);
    }
  }

  fn run(mut self) -> Result<()> {
    let mut data = vec![0; 8192];
    let show_progress = !self.progress.is_hidden();

    loop {
      let len = unix::unistd::read(self.pipe, &mut data)?;
      if len == 0 {
        break;
      }
      self.file.write_all(&data[..len])?;

      // the log line display logic is moderately expensive
      if show_progress {
        let msg_string = String::from_utf8_lossy(&data[..len]);
        for c in msg_string.chars() {
          if c == '\n' {
            self.handle_line();
            self.current_line.clear();
          } else if !c.is_ascii_control() {
            // control characters fuck up the progress bar
            self.current_line.push(c);
          }
        }
      }
    }

    Ok(self.file.flush()?)
  }
}

fn exec_builder(
  store: Arc<dyn Store>,
  scope: &Scope<'_>,
  path: &StorePath,
  drv: &Derivation,
  progress: ProgressBar,
) -> Result<()> {
  let rewrites = drv
    .outputs
    .iter()
    .map(|(name, output)| {
      (
        crate::derivation::hash_placeholder(name),
        store.print_store_path(&output.path),
      )
    })
    .collect::<HashMap<_, _>>();

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

  scope.spawn(move |_| {
    let logger = Logger::new(build_log_path, pipe_read, progress)?;
    logger.run()?;
    <Result<()>>::Ok(())
  });

  let stat = cmd.status()?;
  if stat.success() {
    Ok(())
  } else {
    Err(anyhow!("{} failed with status {:?}", path, stat))
  }
}
