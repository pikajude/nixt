use super::*;
use structopt::StructOpt;

#[derive(StructOpt)]
struct CliSettings {
  #[structopt(
    short = "j",
    long = "build-max-jobs",
    name = "jobs",
    help = "Maximum number of parallel build jobs. \"auto\" means use number of cores.",
    parse(try_from_str = parse_jobs)
  )]
  build_max_jobs: Option<usize>,
}

fn parse_jobs(s: &str) -> Result<usize, <usize as std::str::FromStr>::Err> {
  if s == "auto" {
    Ok(num_cpus::get())
  } else {
    s.parse()
  }
}

impl Settings {
  pub(super) fn parse(&mut self) {
    if cfg!(test) {
      // this stuff confuses libtest, and we set options in tests manually anyway
      return;
    }

    let CliSettings { build_max_jobs, .. } = CliSettings::from_args();
    if let Some(b) = build_max_jobs {
      self.max_build_jobs = b;
    }
  }
}
