let
  fromEnv = var: def:
    let val = builtins.getEnv var; in
    if val != "" then val else def;
in rec {
  shell = "/nix/store/hrpvwkjz04s9i4nmli843hyw9z4pwhww-bash-4.4-p23/bin/bash";
  coreutils = "/nix/store/x0jla3hpxrwz76hy9yckg1iyc9hns81k-coreutils-8.31/bin";
  bzip2 = "/nix/store/rw96psqzgyqrcd12qr6ivk9yiskjm3ab-bzip2-1.0.6.0.1-bin/bin/bzip2";
  gzip = "/nix/store/kkvgr3avpp7yd5hzmc4syh43jqj03sgb-gzip-1.10/bin/gzip";
  xz = "/nix/store/mm0w8jc58rn01c4kz2n9jvwd6bibcihs-xz-5.2.4-bin/bin/xz";
  tar = "/nix/store/g7dr83wnkx4gxa5ykcljc5jg04416z60-gnutar-1.32/bin/tar";
  tarFlags = "--warning=no-timestamp";
  tr = "/nix/store/x0jla3hpxrwz76hy9yckg1iyc9hns81k-coreutils-8.31/bin/tr";
  nixBinDir = fromEnv "NIX_BIN_DIR" "/nix/store/q71m7gb1xx881xix5f9xm4aryq4midvi-nix-2.3.7/bin";
  nixPrefix = "/nix/store/q71m7gb1xx881xix5f9xm4aryq4midvi-nix-2.3.7";
  nixLibexecDir = fromEnv "NIX_LIBEXEC_DIR" "/nix/store/q71m7gb1xx881xix5f9xm4aryq4midvi-nix-2.3.7/libexec";
  nixLocalstateDir = "/nix/var";
  nixSysconfDir = "/etc";
  nixStoreDir = fromEnv "NIX_STORE_DIR" "/nix/store";

  # If Nix is installed in the Nix store, then automatically add it as
  # a dependency to the core packages. This ensures that they work
  # properly in a chroot.
  chrootDeps =
    if dirOf nixPrefix == builtins.storeDir then
      [ (builtins.storePath nixPrefix) ]
    else
      [ ];
}
