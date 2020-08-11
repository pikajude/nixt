with import <nixpkgs> { };
mkShell {
  name = "rnix";
  inputsFrom = [
    (import ../../nix/nix).packages.${builtins.currentSystem}.nix
  ];
  RUST_LIB_BACKTRACE = true;
}
