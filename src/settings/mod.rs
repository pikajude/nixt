#![allow(unused_doc_comments)] // false positive

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

mod cli;

static NIXOS_CACHE_PUBKEY: &str = "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=";

static SETTINGS: Lazy<Settings> = Lazy::new(|| {
  let mut settings = Settings::default();
  settings.parse();
  settings
});

#[derive(Debug, Clone)]
pub struct Paths {
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
}

impl Paths {
  fn get_build_hook(&self) -> PathBuf {
    self.nix_libexec_dir.join("nix").join("build-remote")
  }

  fn builders(&self) -> String {
    format!("@{}", self.nix_conf_dir.join("machines").display())
  }

  fn default_substituters(&self) -> Vec<String> {
    if self.nix_store.to_str() == Some("/nix/store") {
      vec![String::from("https://cache.nixos.org/")]
    } else {
      vec![]
    }
  }

  fn netrc_file(&self) -> PathBuf {
    self.nix_conf_dir.join("netrc")
  }
}

#[settings]
#[derive(Debug, Clone)]
pub struct Settings {
  #[setting(
    value = "Self::default_store()",
    flag = "store",
    help = "The default Nix store to use."
  )]
  pub store_uri: String,

  #[setting(
    value = "false",
    help = "Whether to keep temporary directories of failed builds."
  )]
  pub keep_failed: bool,

  #[setting(
    value = "false",
    help = "Whether to continue building other derivations if one fails."
  )]
  pub keep_going: bool,

  #[setting(
    value = "false",
    alias = "build-fallback",
    help = "Whether to fall back to building when substitution fails."
  )]
  pub try_fallback: bool,

  #[setting(value = "true", hidden)]
  pub verbose_build: bool,

  #[setting(
    value = "10",
    help = "If verbose-build is false, the number of lines of the tail of the log to show if a \
            build fails."
  )]
  pub log_lines: usize,

  #[setting(
    value = "num_cpus::get()",
    help = "Maximum number of parallel build jobs. \"auto\" means use number of cores.",
    flag = "max-jobs",
    alias = "build-max-jobs"
  )]
  pub max_build_jobs: usize,

  #[setting(
    value = "Self::get_default_cores()",
    help = "Number of CPU cores to utilize in parallel within a build, i.e. by passing this \
            number to Make via `-j`. 0 means that the number of actual CPU cores on the local \
            host ought to be auto-detected.",
    flag = "cores",
    alias = "build-cores"
  )]
  pub build_cores: usize,

  #[setting(hidden, value = "false")]
  pub read_only: bool,

  #[setting(
    value = "Self::system().to_string()",
    help = "The canonical Nix system name.",
    flag = "system"
  )]
  pub this_system: String,

  #[setting(
    value = "None",
    help = "The maximum time in seconds that a builer can go without producing any output on \
            stdout/stderr before it is killed. 0 means infinity.",
    alias = "build-max-silent-time"
  )]
  pub max_silent_time: Option<Duration>,

  #[setting(
    value = "None",
    help = "The maximum duration in seconds that a builder can run. 0 means infinity.",
    alias = "build-timeout"
  )]
  pub timeout: Option<Duration>,

  #[setting(
    value = "paths.get_build_hook()",
    help = "The path of the helper program that executes builds on remote machines."
  )]
  pub build_hook: PathBuf,

  #[setting(
    value = "paths.builders()",
    help = "A semicolon-separated list of build machines, in the format of nix.machines."
  )]
  pub builders: String,

  #[setting(
    value = "false",
    help = "Whether build machines should use their own substitutes for obtaining build \
            dependencies if possible, rather than waiting for this host to upload them."
  )]
  pub builders_use_substituters: bool,

  #[setting(
    value = "8 * 1024 * 1024",
    help = "Amount of reserved disk space for the garbage collector. Default: 8MB.",
    flag = "gc-reserved-space"
  )]
  pub reserved_size: u64,

  #[setting(value = "true", help = "Whether SQLite should use fsync().")]
  pub fsync_metadata: bool,

  #[setting(
    value = "!Self::is_wsl1()",
    help = "Whether SQLite should use WAL mode. Default: `false` if running in a WSL 1 container, \
            `true` otherwise."
  )]
  pub use_sqlite_wal: bool,

  #[setting(
    value = "false",
    help = "Whether to call sync() before registering a path as valid."
  )]
  pub sync_before_registering: bool,

  #[setting(
    value = "false",
    help = "Whether to use substitutes.",
    flag = "substitute",
    alias = "build-use-substitutes"
  )]
  pub use_substitutes: bool,

  #[setting(
    value = "Self::build_users_group()",
    help = "The Unix group that contains the build users."
  )]
  pub build_users_group: Option<String>,

  #[setting(
    value = "false",
    help = "Whether to impersonate a Linux 2.6 machine on newer kernels.",
    alias = "build-impersonate-linux-26"
  )]
  pub impersonate_linux_26: bool,

  #[setting(
    value = "true",
    help = "Whether to store build logs.",
    flag = "keep-build-log",
    alias = "build-keep-log"
  )]
  pub keep_log: bool,

  #[setting(
    value = "true",
    help = "Compress logs when storing them.",
    flag = "compress-build-log",
    alias = "build-compress-log"
  )]
  pub compress_log: bool,

  #[setting(
    value = "None",
    help = "Maximum number of bytes a builder can write to stdout and stderr before being killed \
            (0 means no limit).",
    flag = "max-build-log-size",
    alias = "build-max-log-size"
  )]
  pub max_log_size: Option<usize>,

  #[setting(hidden, value = "true")]
  pub print_repeated_builds: bool,

  #[setting(
    value = "Duration::from_secs(5)",
    help = "How often (in seconds) to poll for locks.",
    flag = "build-poll-interval"
  )]
  pub poll_interval: Duration,

  #[setting(
    value = "false",
    help = "Whether to check if new GC roots can in fact be found by the garbage collector.",
    flag = "gc-check-reachability"
  )]
  pub check_root_reachability: bool,

  #[setting(
    value = "false",
    help = "Whether the garbage collector should keep outputs of live derivations.",
    flag = "keep-outputs",
    alias = "gc-keep-outputs"
  )]
  pub gc_keep_outputs: bool,

  #[setting(
    value = "true",
    help = "Whether the garbage collector should keep derivers of live paths.",
    flag = "keep-derivations",
    alias = "gc-keep-derivations"
  )]
  pub gc_keep_derivations: bool,

  #[setting(
    value = "false",
    help = "Whether to automatically replace files with identical contents with hard links."
  )]
  pub auto_optimise_store: bool,

  #[setting(
    value = "false",
    help = "Whether to add derivations as a dependency of user environments (to prevent them from \
            being GCed).",
    flag = "keep-env-derivations",
    alias = "env-keep-derivations"
  )]
  pub env_keep_derivations: bool,

  #[setting(
    hidden,
    value = r#"env::var("NIX_AFFINITY_HACK").map_or(false, |y| y == "1")"#
  )]
  pub lock_cpu: bool,

  #[setting(
    value = "false",
    help = "Whether to show a stack trace on evaluation errors."
  )]
  pub show_trace: bool,

  #[setting(
    value = "SandboxMode::Enabled",
    help = "Whether to enable sandboxed builds. Can be \"true\", \"false\" or \"relaxed\".",
    alias = "build-use-chroot,build-use-sandbox"
  )]
  pub sandbox_mode: SandboxMode,

  #[setting(
    value = "Self::get_default_sandbox_paths()",
    help = "The paths to make available inside the build sandbox.",
    alias = "build-chroot-dirs,build-sandbox-paths"
  )]
  pub sandbox_paths: HashSet<String>,

  #[setting(
    value = "true",
    help = "Whether to disable sandboxing if the kernel doesn't allow it."
  )]
  pub sandbox_fallback: bool,

  #[setting(
    value = "Default::default()",
    help = "Additional paths to make available inside the build sandbox.",
    alias = "build-extra-chroot-dirs,build-extra-sandbox-paths"
  )]
  pub extra_sandbox_paths: HashSet<String>,

  #[setting(
    value = "0",
    help = "The number of times to repeat a build to verify determinism.",
    alias = "build-repeat"
  )]
  pub build_repeat: usize,

  #[cfg(any(target_os = "linux", doc))]
  #[doc(cfg(target_os = "linux"))]
  #[setting(
    value = "String::from(\"50%\")",
    help = "The size of `/dev/shm` in the build sandbox."
  )]
  pub sandbox_shm_size: String,

  #[cfg(any(target_os = "linux", doc))]
  #[doc(cfg(target_os = "linux"))]
  #[setting(
    value = "PathBuf::from(\"/build\")",
    help = "The build directory inside the sandbox. Default: `/build`"
  )]
  pub sandbox_build_dir: PathBuf,

  #[setting(
    value = "Default::default()",
    help = "Which prefixes to allow derivations to ask for access to (primarily for Darwin).",
    flag = "allowed-impure-host-deps"
  )]
  pub allowed_impure_host_prefixes: HashSet<PathBuf>,

  #[cfg(any(target_os = "macos", doc))]
  #[doc(cfg(target_os = "macos"))]
  #[setting(
    value = "false",
    help = "Whether to log Darwin sandbox access violations to the system log. Useful for \
            debugging the sandbox."
  )]
  pub darwin_log_sandbox_violations: bool,

  #[setting(
    value = "false",
    help = "Whether to run the program specified by the diff-hook setting repeated builds produce \
            a different result. Typically used to plug in diffoscope."
  )]
  pub run_diff_hook: bool,

  #[setting(
    value = "None",
    help = "A program that prints out the differences between the two paths specified on its \
            command line."
  )]
  pub diff_hook: Option<PathBuf>,

  #[setting(
    value = "true",
    help = "Whether to fail if repeated builds produce different output."
  )]
  pub enforce_determinism: bool,

  #[setting(
    value = "vec![String::from(NIXOS_CACHE_PUBKEY)]",
    help = "Trusted public keys for secure substitution.",
    alias = "binary-cache-public-keys"
  )]
  pub trusted_public_keys: Vec<String>,

  #[setting(
    value = "vec![]",
    help = "Secret keys with which to sign local builds."
  )]
  pub secret_key_files: Vec<String>,

  #[setting(
    value = "Duration::from_secs(60 * 60)",
    help = "How long downloaded files are considered up-to-date."
  )]
  pub tarball_ttl: Duration,

  #[setting(
    value = "true",
    help = "Whether to check that any non-content-addressed path added to the Nix store has a \
            valid signature (that is, one signed using a key listed in `trusted-public-keys`."
  )]
  pub require_sigs: bool,

  #[setting(
    value = "Self::get_extra_platforms()",
    help = "Additional platforms that can be built on the local system. These may be supported \
            natively (e.g. armv7 on some aarch64 CPUs or using hacks like qemu-user)."
  )]
  pub extra_platforms: HashSet<String>,

  #[setting(
    value = "Self::get_default_system_features()",
    help = "Optional features that this system supports (like `kvm`)."
  )]
  pub system_features: HashSet<String>,

  #[setting(
    value = "paths.default_substituters()",
    help = "The URIs of substituters (such as https://cache.nixos.org/).",
    alias = "binary-caches"
  )]
  pub substituters: Vec<String>,

  #[setting(
    value = "vec![]",
    help = "Additional URIs of substituters.",
    alias = "extra-binary-caches"
  )]
  pub extra_substituters: Vec<String>,

  #[setting(
    value = "Default::default()",
    help = "Disabled substituters that may be enabled via the substituters option by untrusted \
            users.",
    alias = "trusted-binary-caches"
  )]
  pub trusted_substituters: HashSet<String>,

  #[setting(
    value = "vec![String::from(\"root\")]",
    help = "Which users or groups are trusted to ask the daemon to do unsafe things."
  )]
  pub trusted_users: Vec<String>,

  #[setting(
    value = "Duration::from_secs(3600)",
    help = "The TTL in seconds for negative lookups in the disk cache, i.e binary cache lookups \
            that return an invalid path result.",
    flag = "narinfo-cache-negative-ttl"
  )]
  pub ttl_negative_nar_info_cache: Duration,

  #[setting(
    value = "Duration::from_secs(30 * 24 * 3600)",
    help = "The TTL in seconds for positive lookups in the disk cache, i.e binary cache lookups \
            that return a valid path result.",
    flag = "narinfo-cache-positive-ttl"
  )]
  pub ttl_positive_nar_info_cache: Duration,

  #[setting(
    value = "vec![String::from(\"*\")]",
    help = "Which users or groups are allowed to connect to the daemon."
  )]
  pub allowed_users: Vec<String>,

  #[setting(
    value = "true",
    help = "Whether to print what paths need to be built or downloaded."
  )]
  pub print_missing: bool,

  #[setting(
    value = "None",
    help = "A program to run before each build to set derivation-specific build settings."
  )]
  pub pre_build_hook: Option<PathBuf>,

  #[setting(value = "None", help = "A program to run after each successful build.")]
  pub post_build_hook: Option<PathBuf>,

  #[setting(
    value = "paths.netrc_file()",
    help = "Path to the netrc file used to obtain usernames/passwords for downloads."
  )]
  pub netrc_file: PathBuf,

  #[setting(hidden, value = "Self::get_ca_file()")]
  pub ca_file: Option<PathBuf>,

  #[cfg(any(target_os = "linux", doc))]
  #[doc(cfg(target_os = "linux"))]
  #[setting(
    value = "true",
    help = "Whether to prevent certain dangerous system calls, such as creation of setuid/setgid \
            files or adding ACLs or extended attributes. Only disable this if you're aware of the \
            security implications."
  )]
  pub filter_syscalls: bool,

  #[cfg(any(target_os = "linux", doc))]
  #[doc(cfg(target_os = "linux"))]
  #[setting(
    value = "false",
    help = "Whether builders can acquire new privileges by calling programs with setuid/setgid \
            bits or with file capabilities."
  )]
  pub allow_new_privileges: bool,

  #[setting(
    value = "0",
    help = "Automatically run the garbage collector when free disk space drops below the \
            specified amount."
  )]
  pub min_free: u64,

  #[setting(
    value = "std::u64::MAX",
    help = "Stop deleting garbage when free disk space is above the specified amount."
  )]
  pub max_free: u64,

  #[setting(
    value = "Duration::from_secs(5)",
    help = "Number of seconds between checking free disk space."
  )]
  pub min_free_check_interval: Duration,

  #[setting(
    value = "vec![]",
    help = "Plugins to dynamically load at nix initialization time."
  )]
  pub plugin_files: Vec<PathBuf>,

  #[setting(
    value = "None",
    help = "GitHub access token to get access to GitHub data through the GitHub API for \
            `github:<..>` flakes."
  )]
  pub github_access_token: Option<String>,

  #[setting(value = "vec![]", help = "Experimental Nix features.")]
  pub experimental_features: Vec<String>,

  #[setting(value = "true", help = "Whether to allow dirty Git/Mercurial trees.")]
  pub allow_dirty: bool,

  #[setting(
    value = "true",
    help = "Whether to warn about dirty Git/Mercurial trees."
  )]
  pub warn_dirty: bool,

  #[setting(
    value = "32 * 1024 * 1024",
    help = "Maximum size of NARs before spilling them to disk."
  )]
  pub nar_buffer_size: usize,

  #[setting(
    value = "String::from(\"https://github.com/NixOS/flake-registry/raw/master/flake-registry.json\")",
    help = "Path or URI of the global flake registry."
  )]
  pub flake_registry: String,
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

  pub fn has_experimental_feature<B: Borrow<str>>(&self, feature: &B) -> bool {
    let feature = feature.borrow();
    self.experimental_features.iter().any(|x| feature == x)
  }

  fn default_store() -> String {
    std::env::var("NIX_REMOTE").unwrap_or_else(|_| String::from("auto"))
  }

  fn get_default_cores() -> usize {
    cmp::max(1, num_cpus::get())
  }

  fn get_extra_platforms() -> HashSet<String> {
    if cfg!(all(target_os = "linux", target_arch = "x86_64")) {
      std::iter::once(String::from("i686-linux")).collect()
    } else {
      HashSet::new()
    }
  }

  fn build_users_group() -> Option<String> {
    if unix::unistd::getuid().is_root() {
      Some(String::from("nixbld"))
    } else {
      None
    }
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
      eprintln!("warning: using the bash-static hack");
      std::iter::once(format!(
        "/bin/sh={}",
        concat!(env!("OUT_DIR"), "/bash-static")
      ))
      .collect()
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

fn env_fallback_impl(fallback: impl Into<PathBuf>, vars: &[&str]) -> PathBuf {
  for v in vars {
    if let Some(x) = env::var_os(v) {
      return PathBuf::from(x);
    }
  }
  fallback.into()
}

fn env_fallback(fallback: impl Into<PathBuf>, vars: &[&str]) -> PathBuf {
  let base_path = env_fallback_impl(fallback, vars);
  if cfg!(test) || std::env::var("_NIX_TEST").is_ok() {
    PathBuf::from(std::env::var("_NIX_TEST_PREFIX").unwrap())
      .join(base_path.strip_prefix("/").unwrap())
  } else {
    base_path
  }
}

impl Default for Paths {
  fn default() -> Self {
    let nix_store = env_fallback("/nix/store", &["NIX_STORE_DIR", "NIX_STORE"]);
    let nix_state_dir = env_fallback("/nix/var/nix", &["NIX_STATE_DIR"]);
    let nix_libexec_dir = env_fallback("/usr/local/libexec", &["NIX_LIBEXEC_DIR"]);
    let nix_conf_dir = env_fallback("/etc/nix", &["NIX_CONF_DIR"]);
    Self {
      nix_prefix: env_fallback("/nix", &[]),
      nix_data_dir: env_fallback("/usr/local/share", &["NIX_DATA_DIR"]),
      nix_log_dir: env_fallback("/nix/var/nix/log", &["NIX_LOG_DIR"]),
      nix_user_conf_files: get_user_config_files(),
      nix_bin_dir: env_fallback("/usr/local/bin", &["NIX_BIN_DIR"]),
      nix_man_dir: env_fallback("/usr/local/share/man", &[]),
      nix_daemon_socket_file: env::var_os("NIX_DAEMON_SOCKET_PATH").map_or_else(
        || nix_state_dir.join("daemon-socket").join("socket"),
        PathBuf::from,
      ),
      nix_store,
      nix_state_dir,
      nix_conf_dir,
      nix_libexec_dir,
    }
  }
}

fn get_user_config_files() -> Vec<PathBuf> {
  if let Ok(f) = env::var("NIX_USER_CONF_FILES") {
    return f.split(':').map(PathBuf::from).collect();
  }

  std::iter::once(dirs_next::config_dir().expect("no config dir"))
    .chain(
      env::var("XDG_CONFIG_DIRS")
        .unwrap_or_else(|_| String::new())
        .split(':')
        .map(PathBuf::from),
    )
    .map(|x| x.join("nix").join("nix.conf"))
    .collect()
}

#[test]
fn init_settings() {
  eprintln!("{:#?}", Settings::get());
}
