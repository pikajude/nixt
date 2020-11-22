extern crate lalrpop;
use std::{
  env,
  path::Path,
  process::{Command, Stdio},
};

fn link_static_bash() {
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

fn main() {
  println!("cargo:rerun-if-changed=src/syntax/parse.lalrpop");
  lalrpop::process_root().unwrap();

  string_cache_codegen::AtomType::new("atoms::Ident", "ident!")
    .atoms(&[
      "<with>",
      "outPath",
      "drvPath",
      "type",
      "meta",
      "name",
      "value",
      "system",
      "__overrides",
      "outputs",
      "outputName",
      "__ignoreNulls",
      "file",
      "line",
      "column",
      "__functor",
      "__toString",
      "right",
      "wrong",
      "__structuredAttrs",
      "builder",
      "args",
      "__contentAddressed",
      "outputHash",
      "outputHashAlgo",
      "outputHashMode",
      "recurseForDerivations",
      "description",
      "self",
      "",
    ])
    .write_to_file(&Path::new(&std::env::var("OUT_DIR").unwrap()).join("ident.rs"))
    .unwrap();

  link_static_bash();
}
