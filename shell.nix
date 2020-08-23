with import <nixpkgs> { };
mkShell {
  name = "rnix";
  inputsFrom = [
    (import ../../nix/nix).packages.${builtins.currentSystem}.nix
  ];
  buildInputs = [ cargo-udeps ];
  RUST_LIB_BACKTRACE = true;
  RUST_LOG = "rnix=info";
}
