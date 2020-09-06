use rix::{store::*, util::*};
use std::{path::PathBuf, sync::Arc};
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
enum Op {
  #[structopt(name = "--realise", alias = "-r")]
  Realise {
    #[structopt(parse(from_os_str))]
    buildables: Vec<PathBuf>,
  },
}

fn main() -> Result<()> {
  std::env::set_var("NIX_TEST", "1");
  let args = Op::from_args();

  match args {
    Op::Realise { buildables } => {
      let store = Arc::new(LocalStore::open()?);
      let targets = buildables
        .into_iter()
        .map(|path| {
          Ok(rix::path::PathWithOutputs {
            path: store.parse_store_path(&path.canonicalize()?)?,
            outputs: Default::default(),
          })
        })
        .collect::<Result<Vec<_>>>()?;
      store.build_paths(targets)
    }
  }
}
