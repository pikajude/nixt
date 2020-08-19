use super::*;

impl GoalImpl for DerivationGoal {
  fn setup_chroot(&mut self, store: &dyn Store) -> Result<()> {
    todo!()
  }

  fn exec_child(
    &self,
    store: &dyn Store,
    read_side: PtyMaster,
    write_side: i32,
    sandbox_tmpdir: &PathBuf,
  ) -> Result<Child> {
    todo!()
  }
}

impl super::WorkerImpl for DerivationWorker<'_> {
  fn init_chroot(&self, store: &dyn Store, derivation: &Derivation) -> Result<()> {
    todo!()
  }
}
