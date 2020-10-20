use crate::prelude::*;
use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use petgraph::{
  graph::{DiGraph, NodeIndex},
  EdgeDirection::Incoming,
};
use serde::Deserialize;
use std::{
  collections::{BTreeSet, HashMap},
  fmt::Display,
  fs::File,
  io::BufWriter,
  os::unix::prelude::*,
  process::*,
  sync::Arc,
};
use tempfile::TempDir;
use unix::fcntl::OFlag;

type Ix = NodeIndex<u32>;

#[derive(Debug)]
struct Build {
  child: Child,
  progress: ProgressBar,
}

#[derive(Debug)]
enum Child {
  Proc {
    proc: std::process::Child,
    build_dir: TempDir,
  },
  Builtin {
    task: Task<Result<()>>,
  },
}

impl Drop for Child {
  fn drop(&mut self) {
    if let Self::Proc { proc, .. } = self {
      let _ = proc.kill();
    }
  }
}

impl Build {
  fn try_wait(&mut self) -> Result<Option<ExitStatus>> {
    match &mut self.child {
      Child::Proc { proc, .. } => Ok(proc.try_wait()?),
      Child::Builtin { task, .. } => {
        if task.is_panicked() {
          Ok(Some(ExitStatus::from_raw(1)))
        } else if task.is_completed() {
          Ok(Some(ExitStatus::from_raw(0)))
        } else {
          Ok(None)
        }
      }
    }
  }
}

pub struct Worker<'a> {
  goals: DiGraph<Goal, ()>,
  derivations: HashMap<StorePath, Ix>,
  progress: Arc<MultiProgress>,
  store: &'a dyn Store,
  top: Vec<Ix>,
}

#[derive(Debug)]
pub struct Goal {
  path: StorePath,
  derivation: Derivation,
  outputs: BTreeSet<String>,
  build: Option<Build>,
}

impl Display for Goal {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    self.derivation.name.fmt(f)
  }
}

impl<'a> Worker<'a> {
  pub fn with_store(store: &'a dyn Store) -> Self {
    Self {
      store,
      goals: Default::default(),
      top: Default::default(),
      derivations: Default::default(),
      progress: Arc::new(MultiProgress::new()),
    }
  }

  pub fn add_needed(&mut self, path: &StorePath, outputs: &BTreeSet<String>) -> Result<Ix> {
    let ix = self.add(path, outputs)?;
    self.top.push(ix);
    Ok(ix)
  }

  fn add(&mut self, path: &StorePath, outputs: &BTreeSet<String>) -> Result<Ix> {
    let ix = self.goals.add_node(Goal {
      path: path.clone(),
      derivation: Derivation::get(self.store, path)?,
      outputs: outputs.clone(),
      build: None,
    });
    Ok(ix)
  }

  pub fn build(mut self) -> Result<()> {
    for ix in self.goals.node_indices() {
      self.build_graph(ix)?;
    }

    let mut job_count = 0usize;
    let jobs_max = 8;
    // let jobs_max = settings().max_build_jobs;

    // force the multi-bar to stay alive until all builds are finished
    let all_jobs = self.progress.add(
      ProgressBar::new(self.goals.node_count() as _).with_style(
        ProgressStyle::default_bar()
          .template("[{bar:40}] {percent}% {pos}/{len}")
          .progress_chars("=> "),
      ),
    );

    let p2 = Arc::clone(&self.progress);

    std::thread::spawn(move || p2.join_and_clear());

    'poll: loop {
      if self.goals.node_count() == 0 {
        // self.progress.println("all done!");
        break 'poll;
      }

      for node in petgraph::algo::toposort(&self.goals, None).unwrap() {
        let mut to_remove = None;
        let mut in_progress = false;

        if let Some(b) = self.goals[node].build.as_mut() {
          in_progress = true;
          if let Some(status) = b.try_wait()? {
            if status.success() {
              for path in self.goals[node].derivation.outputs.values() {
                self.store.register_valid_path(ValidPathInfo::new(
                  path.path.clone(),
                  Hash::hash_str("foo", HashType::SHA256),
                ))?;
              }
            } else {
              all_jobs.println(format!(
                "builder for {} failed",
                self.goals[node].derivation.name
              ));
              let build_log_path = self.store.logfile_of(&self.goals[node].path);
              if build_log_path.exists() {
                let contents = std::fs::read_to_string(&build_log_path)?;
                for line in contents.lines().rev().take(40).collect::<Vec<_>>() {
                  all_jobs.println(line);
                }
              }
              bail!("build failure for {}", self.goals[node].derivation.name);
            }

            to_remove = Some(node);
          }
        }

        if let Some(x) = to_remove.take() {
          let goal = self.goals.remove_node(x).unwrap();
          job_count -= 1;
          goal.build.unwrap().progress.finish_and_clear();
          all_jobs.inc(1);

          // reset the outer loop, since the toposort needs to be performed again
          continue 'poll;
        }

        if !in_progress
          && job_count < jobs_max
          && self
            .goals
            .neighbors_directed(node, Incoming)
            .next()
            .is_none()
        {
          let build = self.run_builder(&self.goals[node], job_count)?;
          self.goals[node].build = Some(build);
          job_count += 1;
          all_jobs.enable_steady_tick(1000);
        }
      }

      std::thread::sleep(Duration::from_millis(100));
    }

    all_jobs.finish_and_clear();

    Ok(())
  }

  fn build_graph(&mut self, ix: Ix) -> Result<()> {
    for (path, outputs) in self.goals[ix].derivation.input_derivations.clone() {
      if let Some(i) = self.derivations.get(&path).copied() {
        self.goals.update_edge(i, ix, ());
        continue;
      }
      let drv = Derivation::get(self.store, &path)?;
      for out in &outputs {
        if !self.store.is_valid_path(&drv.outputs[out].path)? {
          for v in drv.outputs.values() {
            rmdir(self.store.to_real_path(&v.path)?)?;
          }
          let new_item = self.add_needed(&path, &outputs)?;
          self.derivations.insert(path.clone(), new_item);
          self.build_graph(new_item)?;
          self.goals.update_edge(new_item, ix, ());
          break;
        }
      }
    }
    Ok(())
  }

  fn run_builder(&self, goal: &Goal, progress_bar_index: usize) -> Result<Build> {
    assert!(
      goal.build.is_none(),
      "shouldn't be using run_builder for a goal that is in progress"
    );
    let drv = &goal.derivation;
    let store = self.store;

    let progress = self.progress.insert(
      progress_bar_index,
      ProgressBar::new(1).with_style(
        ProgressStyle::default_spinner().template("[{elapsed}] {prefix:.green} {wide_msg}"),
      ),
    );
    progress.set_prefix(&goal.derivation.name);
    progress.enable_steady_tick(500);
    progress.set_message("building");

    if drv.is_builtin() {
      return exec_builtin(drv, progress);
    }

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

    let build_log_path = self.store.logfile_of(&goal.path);

    std::fs::create_dir_all(build_log_path.parent().unwrap())?;

    let (pipe_read, pipe_write) = unix::unistd::pipe2(OFlag::O_CLOEXEC)?;

    cmd.stdin(Stdio::null());
    unsafe {
      cmd.stdout(Stdio::from_raw_fd(pipe_write));
      cmd.stderr(Stdio::from_raw_fd(pipe_write));
    }

    read_log(build_log_path, pipe_read, progress.clone());

    Ok(Build {
      child: Child::Proc {
        proc: cmd.spawn()?,
        build_dir: builder_tmp,
      },
      progress,
    })
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

fn read_log(log_path: PathBuf, stdout: RawFd, job_progress: ProgressBar) {
  std::thread::spawn(move || {
    if let Err(e) = Logger::new(log_path, stdout, job_progress).and_then(|l| l.run()) {
      panic!("while running logger: {:?}", e);
    }
  });
}

fn exec_builtin(drv: &Derivation, progress: ProgressBar) -> Result<Build> {
  if drv.builder.to_str() == Some("builtin:fetchurl") {
    let drv2 = drv.clone();
    let handle = Task::run(move || crate::fetch::fetchurl(&drv2));
    Ok(Build {
      progress,
      child: Child::Builtin { task: handle },
    })
  } else {
    bail!("unknown builtin: {}", drv.builder.display());
  }
}
