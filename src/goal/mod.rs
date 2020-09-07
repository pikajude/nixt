use crate::{prelude::*, store::LocalStore};
use derivation::DerivationGoal;
use downcast_rs::Downcast;
use indicatif::{ProgressBar, ProgressStyle};
use std::{
  cell::RefCell,
  collections::HashMap,
  fmt::Debug,
  os::unix::io::RawFd,
  rc::Weak,
  sync::Arc,
  time::{Duration, Instant},
};
use unix::{
  poll::{poll, PollFd, PollFlags},
  unistd::read,
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
  fn waitee_done(&mut self, ptr: &GoalPtr) -> Result<Vec<Action>>;
  fn waiters(&self) -> &[WeakGoal];
  fn key(&self) -> String;
  fn name(&self) -> Cow<str>;
  fn handle_eof(&mut self) -> Result<Vec<Action>> {
    todo!()
  }
  fn handle_output(&mut self, _: &ProgressBar, _: &[u8]) -> Result<Vec<Action>> {
    todo!()
  }
}
impl_downcast!(GoalLike);

impl GoalLike for SubstitutionGoal {
  fn work(&mut self, _: &mut Worker) -> Result<Vec<Action>> {
    todo!()
  }

  fn add_waiter(&mut self, ptr: &GoalPtr) {
    self.waiters.push(Rc::downgrade(ptr))
  }

  fn key(&self) -> String {
    todo!()
  }

  fn name(&self) -> Cow<str> {
    todo!()
  }

  fn waiters(&self) -> &[WeakGoal] {
    &self.waiters
  }

  fn waitee_done(&mut self, _: &GoalPtr) -> Result<Vec<Action>> {
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

type Shared<T> = Rc<RefCell<T>>;

type GoalPtr = Shared<dyn GoalLike>;
type WeakGoal = Weak<RefCell<dyn GoalLike>>;
type WeakGoalMap = HashMap<StorePath, WeakGoal>;

pub enum Action {
  AddToWaiters(GoalPtr),
  Wakeup,
  WaitForAwhile,
  WaitForBuildSlot,
  RegisterChild(Child),
  ChildTerminated { wake_sleepers: bool },
  Done(Option<Error>),
}

pub struct Worker {
  store: Arc<LocalStore>,

  progress: ProgressBar,

  top_goals: Vec<GoalPtr>,
  awake: Vec<WeakGoal>,
  wanting_to_build: Vec<WeakGoal>,
  children: Vec<ChildGoal>,

  local_builds: usize,

  derivation_goals: WeakGoalMap,
  substitution_goals: WeakGoalMap,
  _waiting_for_any_goals: Vec<WeakGoal>,
  waiting_for_awhile: Vec<WeakGoal>,

  last_woken_up: Option<Instant>,
  _path_contents_good_cache: HashMap<StorePath, bool>,
}

impl Worker {
  pub fn new(store: Arc<LocalStore>) -> Self {
    let prog = ProgressBar::new(0);
    prog.set_style(
      ProgressStyle::default_bar()
        .template("[{elapsed_precise}] {wide_bar} {pos:>7}/{len:7} {msg}"),
    );
    Self {
      store,
      progress: prog,
      top_goals: Default::default(),
      awake: Default::default(),
      wanting_to_build: Default::default(),
      children: Default::default(),
      local_builds: 0,
      derivation_goals: Default::default(),
      substitution_goals: Default::default(),
      _waiting_for_any_goals: Default::default(),
      waiting_for_awhile: Default::default(),
      last_woken_up: None,
      _path_contents_good_cache: Default::default(),
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
      match upgraded.borrow_mut().downcast_mut::<DerivationGoal>() {
        Some(d) => d.add_wanted_outputs(wanted_outputs),
        _ => bail!("incompatible goal type"),
      }
      Ok(upgraded)
    } else {
      let mk_goal: Shared<dyn GoalLike> = Rc::new(RefCell::new(DerivationGoal::new(
        path.clone(),
        wanted_outputs.into_iter().collect(),
        build_mode,
      )));
      self.derivation_goals.insert(path, Rc::downgrade(&mk_goal));
      self.wake_up(Rc::downgrade(&mk_goal));
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
      let mk_goal: Shared<dyn GoalLike> = Rc::new(RefCell::new(SubstitutionGoal {
        path: path.clone(),
        repair,
        waitees: vec![],
        waiters: vec![],
      }));
      self
        .substitution_goals
        .insert(path, Rc::downgrade(&mk_goal));
      self.wake_up(Rc::downgrade(&mk_goal));
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
        awake2.sort_unstable_by_key(|g| g.borrow().key());
        for goal in awake2 {
          let mut g_lock = goal.borrow_mut();
          info!("working on: {}", g_lock.key());
          for req in g_lock.work(&mut self)? {
            self.act(&goal, req)?;
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
        self.wait_for_input()?;
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

  fn wait_for_input(&mut self) -> Result<()> {
    trace!("waiting for children");

    let mut timeout = None;
    let before = Instant::now();
    let mut deadline = None;
    if settings().min_free != 0 {
      deadline = Some(before + Duration::from_secs(10));
    }

    for c in &self.children {
      if !c.child.respect_timeouts {
        continue;
      }
      if let Some(m) = settings().max_silent_time {
        deadline = Some(try_min(deadline, c.last_output + m));
      }
      if let Some(b) = settings().timeout {
        deadline = Some(try_min(deadline, c.started_at + b));
      }
    }

    if let Some(d) = deadline {
      timeout = Some(std::cmp::max(Duration::from_secs(1), d - before));
    }

    if !self.waiting_for_awhile.is_empty() {
      if self.last_woken_up.map_or(true, |x| x > before) {
        self.last_woken_up = Some(before);
      }
      timeout = Some(std::cmp::max(
        Duration::from_secs(1),
        self.last_woken_up.unwrap() + settings().poll_interval - before,
      ));
    } else {
      self.last_woken_up = None;
    }

    if let Some(t) = timeout {
      trace!("sleeping {:?}", t);
    }

    let mut poll_status = vec![];
    let mut poll_ix = vec![];

    for (i, c) in self.children.iter().enumerate() {
      for (j, fd) in c.child.fds.iter().enumerate() {
        poll_status.push(PollFd::new(*fd, PollFlags::POLLIN));
        poll_ix.push((i, j));
      }
    }

    poll(
      &mut poll_status,
      timeout.map_or(-1, |d| d.as_millis() as i32),
    )?;

    let after = Instant::now();

    for (i, _) in poll_status
      .into_iter()
      .enumerate()
      .filter(|(_, x)| x.revents().is_some())
    {
      let child_ix = poll_ix[i];
      let child = &mut self.children[child_ix.0];
      let child_fd = child.child.fds[child_ix.1];

      let goal_ptr = child.goal.upgrade().unwrap();
      let mut goal = goal_ptr.borrow_mut();

      let mut buffer = vec![0u8; 4096];
      let rd = read(child_fd, &mut buffer).or_else(|e| {
        if e.as_errno() == Some(Errno::EIO) {
          Ok(0)
        } else {
          Err(e)
        }
      })?;

      let actions = if rd == 0 {
        child.child.fds.retain(|x| *x != child_fd);
        goal.handle_eof()?
      } else {
        trace!("{} read {} bytes", goal.name(), rd);
        child.last_output = after;
        self.progress.set_message(&goal.name());
        goal.handle_output(&self.progress, &buffer[..rd])?
      };

      for act in actions {
        self.act(&goal_ptr, act)?;
      }
    }

    if !self.waiting_for_awhile.is_empty()
      && self
        .last_woken_up
        .map_or(false, |l| l + settings().poll_interval <= after)
    {
      self.last_woken_up = Some(after);
      for g in self.waiting_for_awhile.drain(..) {
        if g.upgrade().is_some() {
          self.awake.push(g);
        }
      }
    }

    Ok(())
  }

  fn act(&mut self, goal: &GoalPtr, act: Action) -> Result<()> {
    match act {
      Action::AddToWaiters(w) => {
        w.borrow_mut().add_waiter(&goal);
      }
      Action::Wakeup => {
        self.progress.inc_length(2);
        self.awake.push(Rc::downgrade(&goal));
      }
      Action::WaitForAwhile => {
        self.waiting_for_awhile.push(Rc::downgrade(&goal));
      }
      Action::WaitForBuildSlot => {
        if self.local_builds < settings().max_build_jobs {
          self.awake.push(Rc::downgrade(&goal));
        } else {
          self.wanting_to_build.push(Rc::downgrade(&goal));
        }
      }
      Action::RegisterChild(child) => {
        let c = ChildGoal {
          goal: Rc::downgrade(&goal),
          goal2: Rc::as_ptr(&goal),
          child,
          last_output: Instant::now(),
          started_at: Instant::now(),
        };
        if c.child.in_build_slot {
          self.local_builds += 1;
        }
        self.progress.inc(1);
        self.children.push(c)
      }
      Action::ChildTerminated { wake_sleepers } => {
        let goal_raw = Rc::as_ptr(&goal);
        if let Some(i) = self.children.iter().position(|x| x.goal2 == goal_raw) {
          let child = &self.children[i];
          if child.child.in_build_slot {
            assert!(self.local_builds > 0);
            self.local_builds -= 1;
          }
          self.progress.inc(1);
          self.children.remove(i);
          if wake_sleepers {
            for ptr in self.wanting_to_build.drain(..) {
              if ptr.upgrade().is_some() {
                self.awake.push(ptr);
              }
            }
          }
        }
      }
      Action::Done(e) => match e {
        Some(err) => bail!(err),
        None => {
          let mut acts = vec![];
          for g in goal.borrow().waiters() {
            if let Some(x) = g.upgrade() {
              acts.extend(x.borrow_mut().waitee_done(goal)?);
            }
          }
          for act in acts {
            self.act(goal, act)?;
          }
        }
      },
    }
    Ok(())
  }
}

fn try_min<A: Ord + Copy>(lhs: Option<A>, rhs: A) -> A {
  lhs.map_or(rhs, |x| std::cmp::min(x, rhs))
}

impl Drop for Worker {
  fn drop(&mut self) {
    for g in self.top_goals.drain(..) {
      if Rc::strong_count(&g) > 1 {
        debug!("remaining goal with strong reference: {:?}", g);
      }
    }
  }
}

pub struct Child {
  respect_timeouts: bool,
  in_build_slot: bool,
  fds: Vec<RawFd>,
}

struct ChildGoal {
  child: Child,
  goal: WeakGoal,
  // original nix has this, but idk what it does
  goal2: *const RefCell<dyn GoalLike>,
  last_output: Instant,
  started_at: Instant,
}
