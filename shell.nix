with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "rust-debug";
  buildInputs = [ lldb openssl pkg-config ];
  RUST_MIN_STACK = 10485760;
}
