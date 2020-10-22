use crate::util::*;
use indicatif::ProgressBar;
use serde::Deserialize;
use std::{
  fs::File,
  io::{BufWriter, Write},
  os::unix::prelude::*,
  path::Path,
};

pub struct Logger {
  pipe: RawFd,
  file: BufWriter<File>,
  progress: ProgressBar,
  current_line: String,
  phase: Option<String>,
}

impl Logger {
  pub fn new<P: AsRef<Path>>(path: P, pipe: RawFd, progress: ProgressBar) -> Result<Self> {
    Ok(Self {
      pipe,
      file: BufWriter::new(File::create(path)?),
      progress,
      current_line: String::new(),
      phase: None,
    })
  }

  fn handle_line(&mut self) {
    #[derive(Deserialize)]
    #[serde(rename_all = "camelCase", tag = "action")]
    enum LogMsg {
      Start {
        #[serde(rename = "type")]
        _type: String,
      },
      SetPhase {
        phase: String,
      },
    }

    let l = &self.current_line;
    if let Some(msg) = l.strip_prefix("@nix ") {
      if let Ok(msg) = serde_json::from_str::<LogMsg>(msg) {
        #[allow(clippy::single_match)]
        match msg {
          LogMsg::SetPhase { phase } => {
            self.phase = Some(phase);
          }
          _ => {}
        }
      } else {
        self
          .progress
          .println(&format!("bad JSON message from builder: {:?}", l));
      }
    } else if let Some(ref p) = self.phase {
      self.progress.set_message(&format!("[{}] {}", p, l));
    } else {
      self.progress.set_message(l);
    }
  }

  pub fn run(mut self) -> Result<()> {
    let mut data = vec![0; 8192];
    let show_progress = !self.progress.is_hidden();

    loop {
      let len = unix::unistd::read(self.pipe, &mut data)?;
      if len == 0 {
        break;
      }
      self.file.write_all(&data[..len])?;

      // the log line display logic is moderately expensive
      if show_progress {
        let msg_string = String::from_utf8_lossy(&data[..len]);
        for c in msg_string.chars() {
          if c == '\n' {
            self.handle_line();
            self.current_line.clear();
          } else if !c.is_ascii_control() {
            // control characters fuck up the progress bar
            self.current_line.push(c);
          }
        }
      }
    }

    Ok(self.file.flush()?)
  }
}
