extern crate lalrpop;
use std::{
  env,
  path::Path,
  process::{Command, Stdio},
};

fn main() {
  println!("cargo:rerun-if-changed=src/syntax/parse.lalrpop");
  lalrpop::process_root().unwrap();

  let out_dir = env::var("OUT_DIR").unwrap();
  let bash_path = Path::new(&out_dir).join("bash-static");
  println!("cargo:rerun-if-changed={}", bash_path.display());
  if bash_path.is_file() {
    return;
  }

  println!("cargo:warning=rix currently requires a static version of bash to run builders.");
  println!("cargo:warning=attempting to build one with nix-build...");
  let cmd_output = Command::new("nix-build")
    .args(&["<nixpkgs>", "-A", "pkgsStatic.bash"])
    .stderr(Stdio::inherit())
    .output()
    .expect("unable to execute nix-build");
  let result_path = String::from_utf8(cmd_output.stdout)
    .unwrap_or_else(|_| String::from("unable to parse output of nix-build"));
  let p = Path::new(result_path.trim_end()).join("bin/bash");
  if p.is_file() {
    std::fs::copy(p, bash_path).expect("unable to copy static bash to target directory");
  } else {
    panic!("nix-build failed to produce a correct version of bash")
  }
}
