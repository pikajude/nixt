use rix::{
  eval::{builtins::strings::coerce_new_string, Eval},
  path::PathWithOutputs,
  settings::{CliOptions, Settings},
  util::*,
  Store,
};
use structopt::StructOpt;

#[derive(StructOpt)]
struct Args {
  #[structopt(short = "E", long = "eval")]
  expr: Option<String>,
  #[structopt(name = "DRV-OR-FILE", multiple = true)]
  buildables: Vec<String>,
  #[structopt(flatten)]
  other_opts: CliOptions,
}

fn main() -> Result<()> {
  std::env::set_var("_NIX_TEST", "1");
  rix::globals::init()?;

  let args = Args::from_args();

  Settings::init_with_args(args.other_opts);

  let eval = Eval::new()?;

  let mut build_targets = vec![];

  if let Some(expr) = args.expr {
    let realised = eval.load_inline(expr)?;
    match coerce_new_string(&eval, realised, Default::default()) {
      Err(x) => {
        eval.print_error(x)?;
        std::process::exit(1);
      }
      Ok((ctx, _)) => {
        for path in ctx {
          let (drv_path, output) = decode_context(&path);
          let store_path = eval.store.parse_store_path(drv_path)?;
          build_targets.push(PathWithOutputs {
            path: store_path,
            outputs: std::iter::once(output.to_string()).collect(),
          })
        }
      }
    }
  } else {
    for item in args.buildables {
      if eval.store.is_in_store(&std::path::Path::new(&item)) {
        let store_path = eval.store.parse_store_path(item)?;
        let drv = eval.store.read_derivation(&store_path)?;
        build_targets.push(PathWithOutputs {
          path: store_path,
          outputs: drv.outputs.keys().cloned().collect(),
        });
      } else {
        bail!("not sure how to build item/path {}", item);
      }
    }
  }

  if build_targets.is_empty() {
    bail!("no build targets given on command line")
  }

  eval.store.build_paths(build_targets)?;

  Ok(())
}
