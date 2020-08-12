{ system ? builtins.currentSystem }:
let
  moz_overlay = import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz);
  nixpkgs = import <nixpkgs> { overlays = [ moz_overlay ]; inherit system; };
in
  with nixpkgs;
  let platform = rust.makeRustPlatform {
    rustc = latest.rustChannels.nightly.rust;
    cargo = latest.rustChannels.nightly.cargo;
  }; in
  platform.buildRustPackage rec {
    name = "rnix";
    version = "0.1.0";

    src = builtins.fetchGit ./.;

    cargoSha256 = "1v1279zxl42w5n0fj6fr3bi7j1b7x00lgvdybjza01v5yja5flxl";

    doCheck = false;

    buildInputs = [ sqlite ];

    PKG_CONFIG_PATH = "${libarchive.dev}/lib/pkgconfig";
    PKG_CONFIG = "${pkgconfig}/bin/pkg-config";
  }
