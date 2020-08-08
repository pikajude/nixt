use once_cell::sync::Lazy;
use std::{
  borrow::Borrow,
  cmp,
  collections::HashSet,
  env,
  path::{Path, PathBuf},
  time::Duration,
};
use unix::unistd;

static SETTINGS: Lazy<Settings> = Lazy::new(|| {
  let mut settings = Settings::default();
  settings.apply_overrides();
  settings
});

#[derive(Debug, Clone)]
pub struct Settings {
  pub nix_prefix: PathBuf,
  pub nix_store: PathBuf,
  pub nix_data_dir: PathBuf,
  pub nix_log_dir: PathBuf,
  pub nix_state_dir: PathBuf,
  pub nix_conf_dir: PathBuf,
  pub nix_user_conf_files: Vec<PathBuf>,
  pub nix_libexec_dir: PathBuf,
  pub nix_bin_dir: PathBuf,
  pub nix_man_dir: PathBuf,
  pub nix_daemon_socket_file: PathBuf,
  pub store_uri: String,
  pub keep_failed: bool,
  pub keep_going: bool,
  pub try_fallback: bool,
  pub verbose_build: bool,
  pub log_lines: usize,
  pub max_build_jobs: MaxBuildJobs,
  pub build_cores: usize,
  pub read_only: bool,
  pub this_system: String,
  pub max_silent_time: Option<Duration>,
  pub build_timeout: Option<Duration>,
  pub build_hook: PathBuf,
  pub builders: String,
  pub builders_use_substitutes: bool,
  pub reserved_size: u64,
  pub fsync_metadata: bool,
  pub use_sqlite_wal: bool,
  pub sync_before_registering: bool,
  pub use_substitutes: bool,
  pub build_users_group: Option<String>,
  pub impersonate_linux26: bool,
  pub keep_log: bool,
  pub compress_log: bool,
  pub max_log_size: Option<usize>,
  pub print_repeated_builds: bool,
  pub poll_interval: Duration,
  pub check_root_reachability: bool,
  pub gc_keep_outputs: bool,
  pub gc_keep_derivations: bool,
  pub auto_optimise_store: bool,
  pub keep_env_derivations: bool,
  pub lock_cpu: bool,
  pub sandbox: SandboxMode,
  pub sandbox_paths: HashSet<String>,
  pub sandbox_fallback: bool,
  pub extra_sandbox_paths: HashSet<String>,
  pub build_repeat: usize,
  #[cfg(target_os = "linux")]
  pub sandbox_shm_size: String,
  #[cfg(target_os = "linux")]
  pub sandbox_build_dir: PathBuf,
  pub allowed_impure_host_deps: HashSet<String>,
  #[cfg(target_os = "macos")]
  pub darwin_log_sandbox_violations: bool,
  pub run_diff_hook: bool,
  pub diff_hook: Option<PathBuf>,
  pub enforce_determinism: bool,
  pub trusted_public_keys: Vec<String>,
  pub secret_key_files: Vec<String>,
  pub tarball_ttl: Duration,
  pub require_sigs: bool,
  pub extra_platforms: HashSet<String>,
  pub system_features: HashSet<String>,
  pub substituters: Vec<String>,
  pub extra_substituters: Vec<String>,
  pub trusted_substituters: HashSet<String>,
  pub trusted_users: Vec<String>,
  pub ttl_negative_nar_info_cache: Duration,
  pub ttl_positive_nar_info_cache: Duration,
  pub allowed_users: Vec<String>,
  pub print_missing: bool,
  pub pre_build_hook: Option<PathBuf>,
  pub post_build_hook: Option<PathBuf>,
  pub netrc_file: PathBuf,
  pub ca_file: Option<PathBuf>,
  #[cfg(target_os = "linux")]
  pub filter_syscalls: bool,
  #[cfg(target_os = "linux")]
  pub allow_new_privileges: bool,
  pub min_free: u64,
  pub max_free: u64,
  pub min_free_check_interval: Duration,
  pub plugin_files: Vec<String>,
  pub github_access_token: Option<String>,
  pub experimental_features: Vec<String>,
  pub allow_dirty: bool,
  pub warn_dirty: bool,
  pub nar_buffer_size: usize,
  pub flake_registry: String,
}

#[derive(Debug, Clone)]
pub enum MaxBuildJobs {
  N(usize),
  Auto,
}

#[derive(Debug, Clone, Copy)]
pub enum SandboxMode {
  Enabled,
  Disabled,
  Relaxed,
}

impl Settings {
  pub fn get() -> &'static Self {
    &*SETTINGS
  }

  fn apply_overrides(&mut self) {}

  pub fn has_experimental_feature<B: Borrow<String>>(&self, feature: &B) -> bool {
    let feature = feature.borrow();
    self.experimental_features.iter().any(|x| feature == x)
  }

  fn get_default_cores() -> usize {
    cmp::max(1, num_cpus::get())
  }

  fn system() -> &'static str {
    if cfg!(target_os = "linux") {
      if cfg!(target_arch = "x86_64") {
        "x86_64-linux"
      } else if cfg!(target_arch = "i686") {
        "i686-linux"
      } else {
        unimplemented!("unrecognized system arch")
      }
    } else if cfg!(target_os = "macos") {
      "x86_64-darwin"
    } else {
      unimplemented!("unrecognized target_os")
    }
  }

  fn get_default_system_features() -> HashSet<String> {
    let mut set = vec!["nixos-test", "benchmark", "big-parallel", "recursive-nix"]
      .into_iter()
      .map(String::from)
      .collect::<HashSet<String>>();

    if cfg!(target_os = "linux") {
      use unistd::{access, AccessFlags};
      if access("/dev/kvm", AccessFlags::R_OK | AccessFlags::W_OK).is_ok() {
        set.insert("kvm".into());
      }
    }

    set
  }

  fn is_wsl1() -> bool {
    // WSL1 uses -Microsoft suffix
    // WSL2 uses -microsoft-standard suffix
    unix::sys::utsname::uname()
      .release()
      .ends_with("-Microsoft")
  }

  fn get_ca_file() -> Option<PathBuf> {
    if let Some(x) = env::var_os("NIX_SSL_CERT_FILE") {
      Some(PathBuf::from(x))
    } else if let Some(x) = env::var_os("SSL_CERT_FILE") {
      Some(PathBuf::from(x))
    } else {
      for fname in &[
        "/etc/ssl/certs/ca-certificates.crt",
        "/nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt",
      ] {
        if Path::new(fname).exists() {
          return Some(PathBuf::from(fname));
        }
      }
      None
    }
  }

  fn get_default_sandbox_paths() -> HashSet<String> {
    if cfg!(target_os = "linux") {
      std::iter::once(format!("/bin/sh={}", "foobar")).collect()
    } else if cfg!(target_os = "macos") {
      "/System/Library/Frameworks /System/Library/PrivateFrameworks /bin/sh /bin/bash /private/tmp \
       /private/var/tmp /usr/lib"
        .split_ascii_whitespace()
        .map(String::from)
        .collect()
    } else {
      Default::default()
    }
  }
}

fn env_fallback(fallback: &str, vars: &[&str]) -> PathBuf {
  for v in vars {
    if let Some(x) = env::var_os(v) {
      return PathBuf::from(x);
    }
  }
  PathBuf::from(fallback)
}

impl Default for Settings {
  fn default() -> Self {
    let nix_store = env_fallback("/nix/store", &["NIX_STORE_DIR", "NIX_STORE"]);
    let nix_state_dir = env_fallback("/nix/var/nix", &["NIX_STATE_DIR"]);
    let nix_libexec_dir = env_fallback("/usr/local/libexec", &["NIX_LIBEXEC_DIR"]);
    let nix_conf_dir = env_fallback("/etc/nix", &["NIX_CONF_DIR"]);
    Self {
      nix_prefix: PathBuf::from("/nix"),
      nix_data_dir: env_fallback("/usr/local/share", &["NIX_DATA_DIR"]),
      nix_log_dir: env_fallback("/nix/var/nix/log", &["NIX_LOG_DIR"]),
      nix_user_conf_files: get_user_config_files(),
      nix_bin_dir: env_fallback("/usr/local/bin", &["NIX_BIN_DIR"]),
      nix_man_dir: PathBuf::from("/usr/local/share/man"),
      nix_daemon_socket_file: env::var_os("NIX_DAEMON_SOCKET_PATH").map_or_else(
        || nix_state_dir.join("daemon-socket").join("socket"),
        PathBuf::from,
      ),
      store_uri: env::var("NIX_REMOTE").unwrap_or_else(|_| String::from("auto")),
      keep_failed: false,
      keep_going: false,
      try_fallback: false,
      verbose_build: true,
      log_lines: 10,
      max_build_jobs: MaxBuildJobs::N(1),
      build_cores: Self::get_default_cores(),
      read_only: false,
      this_system: Self::system().to_string(),
      max_silent_time: None,
      build_timeout: None,
      build_hook: nix_libexec_dir.join("nix").join("build-remote"),
      builders: if let Ok(x) = std::env::var("NIX_REMOTE_SYSTEMS") {
        itertools::Itertools::join(&mut x.split(":").map(|x| format!("@{}", x)), " ")
      } else {
        format!("@{}", nix_conf_dir.join("machines").display())
      },
      builders_use_substitutes: false,
      reserved_size: 8 * 1024 * 1024,
      fsync_metadata: true,
      use_sqlite_wal: !Self::is_wsl1(),
      sync_before_registering: false,
      use_substitutes: true,
      build_users_group: if unix::unistd::getuid().is_root() {
        Some(String::from("nixbld"))
      } else {
        None
      },
      impersonate_linux26: false,
      keep_log: true,
      compress_log: true,
      max_log_size: None,
      print_repeated_builds: true,
      poll_interval: Duration::from_secs(5),
      check_root_reachability: false,
      gc_keep_outputs: false,
      gc_keep_derivations: true,
      auto_optimise_store: false,
      keep_env_derivations: false,
      lock_cpu: env::var("NIX_AFFINITY_HACK").map_or(false, |y| y == "1"),
      sandbox: if cfg!(target_os = "linux") {
        SandboxMode::Enabled
      } else {
        SandboxMode::Disabled
      },
      sandbox_paths: Self::get_default_sandbox_paths(),
      sandbox_fallback: true,
      extra_sandbox_paths: Default::default(),
      build_repeat: 0,
      sandbox_shm_size: String::from("50%"),
      sandbox_build_dir: PathBuf::from("/build"),
      allowed_impure_host_deps: Default::default(),
      run_diff_hook: false,
      diff_hook: None,
      enforce_determinism: true,
      trusted_public_keys: vec![String::from(
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=",
      )],
      secret_key_files: vec![],
      tarball_ttl: Duration::from_secs(60 * 60),
      require_sigs: true,
      extra_platforms: if cfg!(target_os = "linux") && !Self::is_wsl1() {
        std::iter::once(String::from("i686-linux")).collect()
      } else {
        Default::default()
      },
      system_features: Self::get_default_system_features(),
      substituters: if nix_store.to_str() == Some("/nix/store") {
        vec!["https://cache.nixos.org".into()]
      } else {
        vec![]
      },
      extra_substituters: Default::default(),
      trusted_substituters: Default::default(),
      trusted_users: vec![String::from("root")],
      ttl_negative_nar_info_cache: Duration::from_secs(3600),
      ttl_positive_nar_info_cache: Duration::from_secs(30 * 24 * 3600),
      allowed_users: vec![String::from("*")],
      print_missing: true,
      pre_build_hook: None,
      post_build_hook: None,
      netrc_file: nix_conf_dir.join("netrc"),
      ca_file: Self::get_ca_file(),
      filter_syscalls: true,
      allow_new_privileges: false,
      min_free: 0,
      max_free: std::u64::MAX,
      min_free_check_interval: Duration::from_secs(5),
      plugin_files: vec![],
      github_access_token: None,
      experimental_features: vec![],
      allow_dirty: true,
      warn_dirty: true,
      nar_buffer_size: 32 * 1024 * 1024,
      flake_registry: String::from(
        "https://github.com/NixOS/flake-registry/raw/master/flake-registry.json",
      ),
      // put these at the bottom, since they're moved here but are borrowed earlier
      nix_store,
      nix_state_dir,
      nix_conf_dir,
      nix_libexec_dir,
    }
  }
}

fn get_user_config_files() -> Vec<PathBuf> {
  if let Ok(f) = env::var("NIX_USER_CONF_FILES") {
    return f.split(":").map(PathBuf::from).collect();
  }

  std::iter::once(dirs_next::config_dir().expect("no config dir"))
    .chain(
      env::var("XDG_CONFIG_DIRS")
        .unwrap_or(String::new())
        .split(":")
        .map(PathBuf::from),
    )
    .map(|x| x.join("nix").join("nix.conf"))
    .collect()
}

#[test]
fn init_settings() {
  eprintln!("{:#?}", Settings::get());
}
