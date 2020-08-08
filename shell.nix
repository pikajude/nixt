with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "rust-debug";
  buildInputs = [ lldb openssl pkg-config libarchive sqlite ];
  RUST_LOG = "rnix=debug";
  LIBCLANG_PATH = "${llvmPackages_7.libclang}/lib";
  CLANG_PATH = "${clangStdenv.cc}/bin/clang";
}
