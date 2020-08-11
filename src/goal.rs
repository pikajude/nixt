use crate::prelude::*;
use settings::SandboxMode;
use std::collections::{HashMap, HashSet};
use unix::sys::stat::Mode;

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum ExitCode {
  Busy,
  Success,
  Failure,
  NoSubstituters,
  IncompleteClosure,
}

pub trait Goal {
  fn status(&self) -> ExitCode;
  fn key(&self) -> String;
}

pub struct DerivationGoal<'a> {
  derivation: Derivation,
  store: &'a dyn Store,
  drv_path: &'a StorePath,
  #[cfg(any(target_os = "macos", doc))]
  extra_sandbox_profile: String,
}

impl<'a> DerivationGoal<'a> {
  pub fn local_build(&self) -> Result<()> {
    if unix::unistd::getuid().is_root() {
      if let Some(_group) = &settings().build_users_group {
        if cfg!(unix) {
        } else {
          bail!("build users are not supported on this platform")
        }
      }
    }

    if !self.derivation.can_build_locally() {
      use itertools::Itertools;
      bail!(
        "a '{}' with features {{{}}} is required to build '{}', but I am a '{}' with features \
         {{{}}}",
        self.derivation.platform,
        self.derivation.required_system_features().iter().join(", "),
        self.store.print_store_path(self.drv_path),
        settings().this_system,
        settings().system_features.iter().join(", ")
      )
    }

    let sandbox_profile = if cfg!(target_os = "macos") {
      self.derivation.env.get("__sandboxProfile")
    } else {
      None
    };

    let no_chroot = self.derivation.env.get("__noChroot") == Some(&String::from("1"));
    let use_chroot = match settings().sandbox_mode {
      SandboxMode::Enabled => {
        if no_chroot {
          bail!(
            "derivation '{}' has `__noChroot' set, but that's not allowed when sandbox = true.",
            self.store.print_store_path(self.drv_path)
          );
        }
        if cfg!(target_os = "macos") && sandbox_profile.is_some() {
          bail!(
            "derivation '{}' specifies a sandbox profile, but that's only allowed when sandbox = \
             relaxed.",
            self.store.print_store_path(self.drv_path)
          );
        }
        true
      }
      SandboxMode::Disabled => false,
      SandboxMode::Relaxed => !no_chroot,
    };

    let host_tmpdir = tmp_build_dir(
      "",
      format!("nix-build-{}", self.drv_path.name),
      false,
      false,
      unsafe { Mode::from_bits_unchecked(0o700) },
    )?;

    let mut input_rewrites = HashMap::new();

    for (key, output) in &self.derivation.outputs {
      input_rewrites.insert(
        crate::derivation::hash_placeholder(key),
        self.store.print_store_path(&output.path),
      );
    }

    let mut env = HashMap::<String, String>::new();

    env.insert(String::from("PATH"), String::from("/path-not-set"));
    env.insert(String::from("HOME"), String::from("/homeless-shelter"));
    env.insert(
      String::from("NIX_STORE"),
      self.store.store_path().to_string_lossy().to_string(),
    );
    env.insert(
      String::from("NIX_BUILD_CORES"),
      format!("{}", settings().build_cores),
    );
    env.insert(String::from("NIX_LOG_FD"), String::from("2"));
    env.insert(String::from("TERM"), String::from("xterm-256color"));

    #[cfg(target_os = "linux")]
    let sandbox_tmpdir = if use_chroot {
      &settings().sandbox_build_dir
    } else {
      &host_tmpdir
    };
    #[cfg(not(target_os = "linux"))]
    let sandbox_tmpdir = &host_tmpdir;

    let pass_as_file: HashSet<_> = self
      .derivation
      .env
      .get("passAsfile")
      .map_or(Default::default(), |x| x.split_ascii_whitespace().collect());

    for (k, v) in &self.derivation.env {
      if pass_as_file.contains(k.as_str()) {
        let attr_hash = Hash::hash_str(k, HashType::SHA256);
        let filename = format!(".attr-{}", attr_hash.encode(Encoding::Base32));
        let filepath = host_tmpdir.join(&filename);
        std::fs::write(&filepath, v.as_bytes())?;
        env.insert(
          format!("{}Path", k),
          sandbox_tmpdir.join(filename).to_string_lossy().to_string(),
        );
      } else {
        env.insert(k.to_string(), v.to_string());
      }
    }

    for alias in &["NIX_BUILD_TOP", "TMPDIR", "TEMPDIR", "TMP", "TEMP", "PWD"] {
      env.insert(String::from(*alias), sandbox_tmpdir.display().to_string());
    }

    if use_chroot {
      let mut dirs_in_chroot = HashMap::<&str, (PathBuf, bool)>::new();

      for chroot_dir in settings()
        .sandbox_paths
        .union(&settings().extra_sandbox_paths)
      {
        let (optional, chroot_dir) = match chroot_dir.strip_suffix('?') {
          Some(c) => (true, c),
          None => (false, chroot_dir.as_str()),
        };

        if let Some((k, v)) = break_str(chroot_dir, '=') {
          dirs_in_chroot.insert(k, (v.into(), optional));
        } else {
          dirs_in_chroot.insert(chroot_dir, (chroot_dir.into(), optional));
        }
      }
    }

    Ok(())
  }
}

fn tmp_build_dir(
  _root: impl AsRef<Path>,
  _prefix: impl AsRef<Path>,
  _include_pid: bool,
  _use_global_counter: bool,
  _mode: Mode,
) -> Result<PathBuf> {
  todo!()
}
