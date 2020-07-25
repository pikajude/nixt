use once_cell::sync::OnceCell;

static SETTINGS: OnceCell<Settings> = OnceCell::new();

#[derive(Debug, Clone)]
pub struct Settings {
  pub read_only: bool,
}

impl Settings {
  pub fn init_default() {
    Self::init(Default::default())
  }

  pub fn init(settings: Self) {
    SETTINGS
      .set(settings)
      .expect("Settings::init() called multiple times")
  }

  pub fn get() -> &'static Self {
    SETTINGS.get().expect("Call Settings::init() first.")
  }
}

impl Default for Settings {
  fn default() -> Self {
    Self { read_only: false }
  }
}
