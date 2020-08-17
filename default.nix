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

    cargoSha256 = "10qnxg8fbpn8a1a138zy68qabfqpvqcqvd0cm5j162fjg86a1505";

    buildInputs = [ sqlite ];

    PKG_CONFIG_PATH = "${libarchive.dev}/lib/pkgconfig";
    PKG_CONFIG = "${pkgconfig}/bin/pkg-config";
  }
