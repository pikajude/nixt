use crate::{prelude::*, Store};
use std::{
  collections::{BTreeSet, HashMap},
  os::unix::prelude::*,
  process::{Command, ExitStatus, Stdio},
};

pub fn build_derivation<S: Store>(
  store: &S,
  path: &StorePath,
  outputs: &BTreeSet<String>,
) -> Result<()> {
  let drv = store.read_derivation(path)?;

  let mut must_build = false;

  let mut rewrites = HashMap::new();

  for out in outputs {
    let p = &drv.outputs[out].path;
    if !store.is_valid_path(p)? {
      debug!("path {} must be built", p);
      must_build = true;
    }
    rewrites.insert(
      crate::derivation::hash_placeholder(out),
      store.print_store_path(&drv.outputs[out].path),
    );
  }

  if !must_build {
    debug!("outputs of {} are all built already, nothing to do", path);
    return Ok(());
  } else {
    debug!("building {}", path);
  }

  for (path, outputs) in &drv.input_derivations {
    build_derivation(store, path, outputs)?;
  }

  debug!("{:#?}", rewrites);

  let build_status = if drv.is_builtin() {
    if drv.builder.to_str() == Some("builtin:fetchurl") {
      crate::fetch::fetchurl(&drv)?;
      ExitStatus::from_raw(0)
    } else {
      bail!("unknown builtin: {}", drv.builder.display());
    }
  } else {
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

    let builder_tmp = tempfile::tempdir()?;
    cmd.current_dir(&builder_tmp);

    cmd.env("PATH", "/path-not-set");
    cmd.env("HOME", "/homeless-shelter");
    cmd.env("NIX_STORE", dbg!(store.store_path()));
    cmd.env("NIX_BUILD_CORES", settings().build_cores.to_string());
    cmd.env("NIX_LOG_FD", "2");
    cmd.env("TERM", "xterm-256color");

    for alias in &["NIX_BUILD_TOP", "TMPDIR", "TEMPDIR", "TMP", "TEMP", "PWD"] {
      cmd.env(alias, builder_tmp.as_ref().display().to_string());
    }

    let mut log_part0 = path.to_string();
    let log_part1 = log_part0.split_off(2);

    let build_log_path = settings()
      .paths
      .nix_log_dir
      .join("drvs")
      .join(log_part0)
      .join(log_part1);

    std::fs::create_dir_all(build_log_path.parent().unwrap())?;

    let (pipe_read, pipe_write) = unix::unistd::pipe()?;

    info!("running builder in {}", builder_tmp.as_ref().display());

    cmd.stdin(Stdio::null());
    unsafe {
      cmd.stdout(Stdio::from_raw_fd(pipe_write));
      cmd.stderr(Stdio::from_raw_fd(pipe_write));
    }

    debug!("executing builder: {:?}", &cmd);

    let mut child = cmd.spawn()?;

    std::thread::spawn(move || {
      let mut build_log_file = fs::File::create(build_log_path).unwrap();
      loop {
        let mut data = vec![0; 8192];
        let len = unix::unistd::read(pipe_read, &mut data).unwrap();
        if len == 0 {
          break;
        }
        build_log_file.write_all(&data[..len]).unwrap();
        eprint!("{}", String::from_utf8_lossy(&data[..len]));
      }
    });

    let status = child.wait()?;

    unix::unistd::close(pipe_write)?;

    status
  };

  debug!("child has terminated");

  if build_status.success() {
    for path in drv.outputs.values() {
      store.register_valid_path(ValidPathInfo::new(
        path.path.clone(),
        Hash::hash_str("foo", HashType::SHA256),
      ))?;
    }
    Ok(())
  } else {
    warn!("{:?}", build_status);
    bail!("builder for {} failed: {}", path, build_status)
  }
}
