use crate::{prelude::*, sync::fs_lock::*};
use fs::File;
use unix::unistd::{Gid, Uid};
use users::os::unix::GroupExt;

pub struct UserLock {
  _lockfile: File,
  pub uid: Uid,
  pub gid: Gid,
  pub other_gids: Vec<Gid>,
}

impl UserLock {
  pub fn get_free_user(groupname: &str) -> Result<Self> {
    let group = users::get_group_by_name(groupname)
      .ok_or_else(|| anyhow!("the build users group '{}' does not exist", groupname))?;

    let gid = Gid::from_raw(group.gid());

    let members = group.members();

    if members.is_empty() {
      bail!("the build users group '{}' has no members", groupname);
    }

    for user in members {
      debug!("trying user {:?}", user);

      let user = users::get_user_by_name(user).ok_or_else(|| {
        anyhow!(
          "the user {:?} in the group {:?} does not exist",
          user,
          groupname
        )
      })?;

      let filename = settings()
        .paths
        .nix_state_dir
        .join("userpool")
        .join(format!("{}", user.uid()));

      let lockfile = File::create(filename)?;
      if lockfile.try_lock(LockType::Write)? {
        if user.uid() == unix::unistd::getuid().as_raw()
          || user.uid() == unix::unistd::geteuid().as_raw()
        {
          bail!("the Nix user should not be a member of {}", groupname);
        }

        let other_gids = if cfg!(target_os = "linux") {
          users::get_user_groups(user.name(), gid.as_raw()).map_or(vec![], |x| {
            x.into_iter()
              .map(|y| Gid::from_raw(y.gid()))
              .collect::<Vec<_>>()
          })
        } else {
          vec![]
        };

        return Ok(Self {
          _lockfile: lockfile,
          uid: Uid::from_raw(user.uid()),
          gid,
          other_gids,
        });
      }
    }

    bail!("unable to find a free build user")
  }

  pub fn kill(&self) -> Result<()> {
    Ok(())
  }
}
