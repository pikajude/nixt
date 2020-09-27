with import <nixpkgs> { };
mkShell {
  name = "rix";
  buildInputs = [ rustup cargo-udeps libarchive pkg-config sqlite ];
  RUST_LIB_BACKTRACE = true;
  PKG_CONFIG_ALLOW_CROSS = true;
  NIX_TRACE = true;
  RUST_LOG = "rix=debug";
  _NIX_TEST_STORE = "${builtins.toString ./.}/rix-store";
}
