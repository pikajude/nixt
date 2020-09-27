use crate::{prelude::*, Store};
use std::{
  collections::BTreeSet,
  os::unix::{io::*, process::CommandExt},
  process::{Command, Stdio},
};

pub fn build_derivation<S: Store>(
  store: &S,
  path: &StorePath,
  outputs: &BTreeSet<String>,
) -> Result<()> {
  let drv = store.read_derivation(path)?;

  let mut must_build = false;

  for out in outputs {
    let p = &drv.outputs[out].path;
    if !store.is_valid_path(p)? {
      debug!("path {} must be built", p);
      must_build = true;
    }
  }

  if !must_build {
    debug!("outputs of {} are all built already, nothing to do", path);
    return Ok(());
  }

  info!("{:?}", drv);

  for (path, outputs) in &drv.input_derivations {
    build_derivation(store, path, outputs)?;
  }

  if drv.is_builtin() {
    if drv.builder.to_str() == Some("builtin:fetchurl") {
      return crate::fetch::fetchurl(&drv);
    } else {
      bail!("unknown builtin: {}", drv.builder.display());
    }
  }

  let mut cmd = Command::new(drv.builder.as_os_str());
  cmd.arg0(&drv.args[0]);
  cmd.args(&drv.args[1..]);
  cmd.envs(&drv.env);

  let builder_tmp = tempfile::tempdir()?.into_path();
  cmd.current_dir(&builder_tmp);

  info!("running builder in {}", builder_tmp.display());

  cmd.stdin(Stdio::null());
  let fd = std::io::stderr().as_raw_fd();
  unsafe {
    cmd.stdout(Stdio::from_raw_fd(fd));
    cmd.stderr(Stdio::from_raw_fd(fd));
  }

  debug!("executing builder: {:?}", &cmd);

  if cmd.status()?.success() {
    for path in drv.outputs.values() {
      store.register_valid_path(ValidPathInfo::new(
        path.path.clone(),
        Hash::hash_str("foo", HashType::SHA256),
      ))?;
    }
    Ok(())
  } else {
    bail!("builder failure")
  }
}
