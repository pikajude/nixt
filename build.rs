use std::path::PathBuf;

extern crate bindgen;
extern crate lalrpop;
extern crate pkg_config;

fn main() {
  pkg_config::Config::new()
    .atleast_version("3.0")
    .probe("libarchive")
    .unwrap();
  let bindgen = bindgen::Builder::default()
    .header_contents(
      "wrapper.h",
      "#include <archive.h>\n#include <archive_entry.h>",
    )
    .parse_callbacks(Box::new(bindgen::CargoCallbacks))
    .generate()
    .unwrap();
  bindgen
    .write_to_file(PathBuf::from(std::env::var("OUT_DIR").unwrap()).join("bindings.rs"))
    .unwrap();
  lalrpop::process_root().unwrap();
}
