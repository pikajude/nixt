use std::{
  fmt::Debug,
  sync::{Arc, Mutex, TryLockError},
  thread::JoinHandle,
};

pub struct Task<T> {
  handle: JoinHandle<()>,
  cell: Arc<Mutex<Option<T>>>,
}

impl<T: Send + 'static> Task<T> {
  pub fn run<F: FnOnce() -> T + Send + 'static>(task: F) -> Self {
    let cell = Arc::new(Mutex::new(None));
    let c2 = Arc::clone(&cell);
    let handle = std::thread::spawn(move || {
      let mut guard = c2
        .lock()
        .expect("invariant: lock failed with only one consumer");
      let result = task();
      *guard = Some(result);
    });
    Self { handle, cell }
  }
}

impl<T> Task<T> {
  pub fn completed(value: T) -> Self {
    Self {
      handle: std::thread::spawn(move || {}),
      cell: Arc::new(Mutex::new(Some(value))),
    }
  }

  pub fn is_completed(&self) -> bool {
    self.cell.try_lock().is_ok()
  }

  pub fn is_running(&self) -> bool {
    matches!(self.cell.try_lock(), Err(TryLockError::WouldBlock))
  }

  pub fn is_panicked(&self) -> bool {
    matches!(self.cell.try_lock(), Err(TryLockError::Poisoned(_)))
  }

  pub fn wait(self) -> std::thread::Result<T> {
    self.handle.join()?;
    Ok(
      self
        .cell
        .lock()
        .expect("invariant: there should be no producers for this mutex")
        .take()
        .expect("invariant: thread completed successfully, but the cell is empty"),
    )
  }
}

impl<T: Debug> Debug for Task<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut s = f.debug_struct("Task");
    match self.cell.try_lock() {
      Ok(f) => s.field("result", f.as_ref().unwrap()),
      Err(TryLockError::WouldBlock) => s.field("result", &"..."),
      Err(_) => s.field("result", &"<PANIC>"),
    };
    s.finish()
  }
}
