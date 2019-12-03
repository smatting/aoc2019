{ sources ? import ../nix/sources.nix }:

let
  pkgs = import sources."nixpkgs" {};
  haskell = import sources."haskell.nix"  { inherit pkgs; };

  pkgSet = haskell.mkCabalProjectPkgSet {
    plan-pkgs = import ./pkgs.nix;
    pkg-def-extras = [];
    modules = [];
  };

in
  pkgSet.config.hsPkgs
