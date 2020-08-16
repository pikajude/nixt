use crate::prelude::*;
use petgraph::graph::{DiGraph, EdgeIndex, NodeIndex};
use std::{
  iter::FusedIterator,
  sync::{Arc, Mutex},
};

#[derive(Debug)]
pub struct Worker(Arc<Mutex<Inner>>);

impl Worker {
  pub fn new(store: Arc<dyn Store>) -> Self {
    Self(Arc::new(Mutex::new(Inner {
      store,
      goals: DiGraph::new(),
    })))
  }

  pub fn add_goal<G: Goal + 'static>(&self, goal: G) {
    self.0.lock().unwrap().goals.add_node(Box::new(goal));
  }

  pub fn step(&self) -> Result<bool> {
    let mut this = self.0.lock().unwrap();
    let store = Arc::clone(&this.store);
    if let Some(topo_head) = petgraph::algo::toposort(&this.goals, None)
      .map_err(|e| anyhow!("cycle detected at node {:?}", e))?
      .into_iter()
      .next()
    {
      let signal = this.goals[topo_head].step(store)?;
      match signal {
        Signal::Done => {
          this.goals.remove_node(topo_head);
        }
        Signal::Nothing => {}
        Signal::Depends(deps) => {
          for r in deps {
            this.add_waitee(topo_head, r);
          }
        }
      }
      Ok(true)
    } else {
      Ok(false)
    }
  }

  pub fn iter(&self) -> Iter {
    Iter {
      worker: self,
      done: false,
    }
  }

  pub fn run(self) -> Result<()> {
    self.iter().try_for_each(|x| x)
  }
}

pub enum Signal {
  Nothing,
  Done,
  Depends(Vec<Box<dyn Goal>>),
}

#[derive(derivative::Derivative)]
#[derivative(Debug)]
struct Inner {
  #[derivative(Debug = "ignore")]
  store: Arc<dyn Store>,
  goals: DiGraph<Box<dyn Goal>, ()>,
}

impl Inner {
  fn add_waitee(&mut self, ix: NodeIndex<u32>, goal: Box<dyn Goal>) -> EdgeIndex<u32> {
    let new_node = self.goals.add_node(goal);
    self.goals.add_edge(ix, new_node, ())
  }
}

pub trait Goal: std::fmt::Debug {
  fn step(&mut self, store: Arc<dyn Store>) -> Result<Signal>;
}

pub struct Iter<'a> {
  worker: &'a Worker,
  done: bool,
}

impl<'a> Iterator for Iter<'a> {
  type Item = Result<()>;

  fn next(&mut self) -> Option<Self::Item> {
    if self.done {
      return None;
    }
    match self.worker.step() {
      Ok(true) => Some(Ok(())),
      Ok(false) => {
        self.done = true;
        Some(Ok(()))
      }
      Err(e) => Some(Err(e)),
    }
  }
}

impl<'a> FusedIterator for Iter<'a> {}

#[cfg(test)]
mod test_worker {
  use super::*;
  use crate::store::LocalStore;

  #[derive(Debug)]
  enum SimpleGoal {
    Start,
    Middle,
    End,
  }

  impl Goal for SimpleGoal {
    fn step(&mut self, _: Arc<dyn Store>) -> Result<Signal> {
      match self {
        Self::Start => {
          *self = Self::Middle;
          Ok(Signal::Depends(vec![Box::new(SimpleGoal::Start)]))
        }
        _ => {
          *self = Self::End;
          Ok(Signal::Done)
        }
      }
    }
  }

  #[test]
  fn test_work() -> Result<()> {
    let _ = pretty_env_logger::try_init();

    let store = Arc::new(LocalStore::open()?);

    let worker = Worker::new(store);

    worker.add_goal(SimpleGoal::Start);

    worker.iter().take(5).try_for_each(|x| {
      eprintln!("{:?}", worker);
      x
    })?;

    Ok(())
  }
}
