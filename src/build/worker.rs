use crate::prelude::*;
use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use petgraph::{
  graph::{DiGraph, NodeIndex},
  EdgeDirection::Incoming,
};
use std::{
  collections::{BTreeSet, HashMap},
  fmt::Display,
  os::unix::{
    io::FromRawFd,
    process::{CommandExt, ExitStatusExt},
  },
  process::{Command, ExitStatus, Stdio},
  sync::{Arc, Mutex},
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
    status: Arc<Mutex<Option<ExitStatus>>>,
  },
}

impl Build {
  fn try_wait(&mut self) -> std::io::Result<Option<ExitStatus>> {
    match &mut self.child {
      Child::Proc { proc, .. } => proc.try_wait(),
      Child::Builtin { status, .. } => status
        .try_lock()
        .map_err(|_| std::io::Error::new(std::io::ErrorKind::Other, "mutex poisoned"))
        .map(|x| *x),
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

    let mut jobs = BTreeSet::<String>::new();

    // force the multi-bar to stay alive until all builds are finished
    let all_jobs = self
      .progress
      .add(ProgressBar::new(self.goals.node_count() as _));

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
                for line in contents.lines().rev().take(10).collect::<Vec<_>>() {
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
          jobs.remove(&goal.derivation.name);
          goal.build.unwrap().progress.finish_and_clear();
          all_jobs.inc(1);

          // reset the outer loop, since the toposort needs to be performed again
          continue 'poll;
        }

        if !in_progress
          && jobs.len() <= 8
          && self
            .goals
            .neighbors_directed(node, Incoming)
            .next()
            .is_none()
        {
          let build = self.run_builder(&self.goals[node])?;
          self.goals[node].build = Some(build);
          jobs.insert(self.goals[node].derivation.name.to_string());
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
            let _ = std::fs::remove_dir_all(self.store.to_real_path(&v.path)?);
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

  fn run_builder(&self, goal: &Goal) -> Result<Build> {
    assert!(
      goal.build.is_none(),
      "shouldn't be using run_builder for a goal that is in progress"
    );
    let drv = &goal.derivation;
    let store = self.store;

    let progress = self.progress.insert(
      0, // keep the all-progress bar at the bottom
      ProgressBar::new(1).with_style(
        ProgressStyle::default_spinner()
          .template("{spinner} {prefix:.green} [{elapsed}] {wide_msg}"),
      ),
    );
    progress.set_prefix(&goal.path.to_string());
    progress.enable_steady_tick(1000 / 15);
    progress.set_message("building");

    if drv.is_builtin() {
      if drv.builder.to_str() == Some("builtin:fetchurl") {
        let status = Arc::new(Mutex::new(None));
        let drv2 = drv.clone();
        let stat2 = Arc::clone(&status);
        std::thread::spawn(move || {
          let stat = crate::fetch::fetchurl(&drv2).is_ok();
          *stat2.lock().unwrap() = Some(ExitStatus::from_raw(if stat { 0 } else { 1 }))
        });
        return Ok(Build {
          progress,
          child: Child::Builtin { status },
        });
      } else {
        bail!("unknown builtin: {}", drv.builder.display());
      }
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

    let prog2 = progress.clone();

    std::thread::spawn(move || {
      let mut build_log_file = fs::File::create(build_log_path).unwrap();
      loop {
        let mut data = vec![0; 8192];
        let len = unix::unistd::read(pipe_read, &mut data).unwrap();
        if len == 0 {
          break;
        }
        build_log_file.write_all(&data[..len]).unwrap();
        let msg_string = String::from_utf8_lossy(&data[..len]);
        prog2.set_message(msg_string.lines().last().unwrap_or(""));
      }

      build_log_file.flush().unwrap();
    });

    Ok(Build {
      child: Child::Proc {
        proc: cmd.spawn()?,
        build_dir: builder_tmp,
      },
      progress,
    })
  }
}
