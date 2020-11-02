use super::*;
use structopt::StructOpt;

#[derive(StructOpt)]
pub struct CliOptions {
  #[structopt(
    short = "j",
    long = "build-max-jobs",
    name = "jobs",
    help = "Maximum number of parallel build jobs. \"auto\" means use number of cores.",
    parse(try_from_str = parse_jobs)
  )]
  pub build_max_jobs: Option<usize>,
}

fn parse_jobs(s: &str) -> Result<usize, <usize as std::str::FromStr>::Err> {
  if s == "auto" {
    Ok(num_cpus::get())
  } else {
    s.parse()
  }
}

impl Settings {
  pub(super) fn apply_overrides(&mut self, f: CliOptions) {
    if cfg!(test) {
      // this stuff confuses libtest, and we set options in tests manually anyway
      return;
    }

    if let Some(b) = f.build_max_jobs {
      self.max_build_jobs = b;
    }
  }
}
