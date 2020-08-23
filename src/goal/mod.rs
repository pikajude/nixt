use crate::{prelude::*, store::LocalStore};
use derivation::DerivationGoal;
use downcast_rs::Downcast;
use std::{
  collections::HashMap,
  fmt::Debug,
  sync::{Arc, RwLock, Weak},
  time::Instant,
};
mod derivation;

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum BuildMode {
  Normal,
  Repair,
  Check,
}

pub trait GoalLike: Downcast + Debug {
  fn work(&mut self, worker: &mut Worker) -> Result<Vec<Action>>;
  fn add_waiter(&mut self, ptr: &GoalPtr);
  fn key(&self) -> String;
}
impl_downcast!(GoalLike);

impl GoalLike for SubstitutionGoal {
  fn work(&mut self, worker: &mut Worker) -> Result<Vec<Action>> {
    todo!()
  }

  fn add_waiter(&mut self, ptr: &GoalPtr) {
    self.waiters.push(Arc::downgrade(ptr))
  }

  fn key(&self) -> String {
    todo!()
  }
}

#[derive(Debug)]
pub struct SubstitutionGoal {
  pub path: StorePath,
  pub repair: bool,
  pub waitees: Vec<GoalPtr>,
  pub waiters: Vec<WeakGoal>,
}

type Shared<T> = Arc<RwLock<T>>;

type GoalPtr = Shared<dyn GoalLike>;
type WeakGoal = Weak<RwLock<dyn GoalLike>>;
type WeakGoalMap = HashMap<StorePath, WeakGoal>;

pub enum Action {
  AddToWaiters(GoalPtr),
  Wakeup,
  WaitForAwhile,
  WaitForBuildSlot,
}

pub struct Worker {
  store: Arc<LocalStore>,

  top_goals: Vec<GoalPtr>,
  awake: Vec<WeakGoal>,
  wanting_to_build: Vec<WeakGoal>,
  children: Vec<Child>,

  local_builds: usize,

  derivation_goals: WeakGoalMap,
  substitution_goals: WeakGoalMap,
  waiting_for_any_goals: Vec<WeakGoal>,
  waiting_for_awhile: Vec<WeakGoal>,

  last_woken_up: Instant,
  path_contents_good_cache: HashMap<StorePath, bool>,
}

impl Worker {
  pub fn new(store: Arc<LocalStore>) -> Self {
    Self {
      store,
      top_goals: Default::default(),
      awake: Default::default(),
      wanting_to_build: Default::default(),
      children: Default::default(),
      local_builds: 0,
      derivation_goals: Default::default(),
      substitution_goals: Default::default(),
      waiting_for_any_goals: Default::default(),
      waiting_for_awhile: Default::default(),
      last_woken_up: Instant::now(),
      path_contents_good_cache: Default::default(),
    }
  }

  fn wake_up(&mut self, goal: WeakGoal) {
    debug!("waking up {:?}", goal);
    self.awake.push(goal);
  }

  pub fn make_derivation_goal<I: IntoIterator<Item = String>>(
    &mut self,
    path: StorePath,
    wanted_outputs: I,
    build_mode: BuildMode,
  ) -> Result<GoalPtr> {
    if let Some(ptr) = self.derivation_goals.get(&path) {
      let upgraded = ptr.upgrade().ok_or_else(|| {
        anyhow!(
          "unexpected dead weak pointer in goal map for path {:?}",
          &path
        )
      })?;
      match upgraded
        .write()
        .expect("unable to write")
        .downcast_mut::<DerivationGoal>()
      {
        Some(d) => d.add_wanted_outputs(wanted_outputs),
        _ => bail!("incompatible goal type"),
      }
      Ok(upgraded)
    } else {
      let mk_goal: Shared<dyn GoalLike> = Arc::new(RwLock::new(DerivationGoal::new(
        path.clone(),
        wanted_outputs.into_iter().collect(),
        build_mode,
      )));
      self.derivation_goals.insert(path, Arc::downgrade(&mk_goal));
      self.wake_up(Arc::downgrade(&mk_goal));
      Ok(mk_goal)
    }
  }

  pub fn make_substitution_goal(&mut self, path: StorePath, repair: bool) -> Result<GoalPtr> {
    if let Some(x) = self.substitution_goals.get(&path) {
      x.upgrade().ok_or_else(|| {
        anyhow!(
          "unexpected dead weak pointer in goal map for path {:?}",
          &path
        )
      })
    } else {
      let mk_goal: Shared<dyn GoalLike> = Arc::new(RwLock::new(SubstitutionGoal {
        path: path.clone(),
        repair,
        waitees: vec![],
        waiters: vec![],
      }));
      self
        .substitution_goals
        .insert(path, Arc::downgrade(&mk_goal));
      self.wake_up(Arc::downgrade(&mk_goal));
      Ok(mk_goal)
    }
  }

  pub fn run<I: IntoIterator<Item = GoalPtr>>(mut self, goals: I) -> Result<()> {
    self.top_goals.extend(goals.into_iter());

    loop {
      while !self.awake.is_empty() && !self.top_goals.is_empty() {
        let mut awake2 = vec![];
        for ptr in self.awake.drain(..) {
          if let Some(x) = ptr.upgrade() {
            awake2.push(x);
          }
        }
        awake2.sort_unstable_by_key(|g| g.read().unwrap().key());
        for goal in awake2 {
          let mut g_lock = goal.write().expect("unable to lock for writing");
          info!("working on: {}", g_lock.key());
          for req in g_lock.work(&mut self)? {
            match req {
              Action::AddToWaiters(w) => {
                w.write().unwrap().add_waiter(&goal);
              }
              Action::Wakeup => {
                self.awake.push(Arc::downgrade(&goal));
              }
              Action::WaitForAwhile => {
                self.waiting_for_awhile.push(Arc::downgrade(&goal));
              }
              Action::WaitForBuildSlot => {
                if self.local_builds < settings().max_build_jobs {
                  self.awake.push(Arc::downgrade(&goal));
                } else {
                  self.wanting_to_build.push(Arc::downgrade(&goal));
                }
              }
            }
          }
          if self.top_goals.is_empty() {
            break;
          }
        }
      }

      if self.top_goals.is_empty() {
        break;
      }

      if !self.children.is_empty() || !self.waiting_for_awhile.is_empty() {
        todo!("wait for input");
      } else {
        if self.awake.is_empty() && settings().max_build_jobs == 0 {
          todo!("check machines");
        }
        assert!(!self.awake.is_empty());
      }
    }

    let keep_going = settings().keep_going;
    assert!(!keep_going || self.awake.is_empty());
    assert!(!keep_going || self.wanting_to_build.is_empty());
    assert!(!keep_going || self.children.is_empty());

    Ok(())
  }
}

impl Drop for Worker {
  fn drop(&mut self) {
    for g in self.top_goals.drain(..) {
      if Arc::strong_count(&g) > 1 {
        debug!("remaining goal with strong reference: {:?}", g);
      }
    }
  }
}

pub struct Child {
  goal: WeakGoal,
  respect_timeouts: bool,
  in_build_slot: bool,
  last_output: Instant,
  started_at: Instant,
}
