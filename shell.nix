with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "rust-debug";
  buildInputs = [ lldb ];
}
