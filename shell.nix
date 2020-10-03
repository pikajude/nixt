with import <nixpkgs> { };
mkShell {
  name = "rix";
  buildInputs = [ rustup cargo-udeps libarchive pkg-config sqlite openssl ];
  RUST_LIB_BACKTRACE = true;
  PKG_CONFIG_ALLOW_CROSS = true;
  NIX_TRACE = true;
  RUST_LOG = "rix=debug";
  _NIX_TEST_STORE = "${builtins.toString ./.}/rix-store";
  LD_LIBRARY_PATH = "${python38}/lib";
  PYTHONPATH = "${llvmPackages_10.lldb}/lib/python3.8/site-packages";
}
