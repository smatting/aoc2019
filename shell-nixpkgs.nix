let
  sources = import ./nix/sources.nix;
in
  with (import sources."nixpkgs" {});
  let
    ghc = (haskell.packages.ghc865.ghcWithPackages (hs: [ hs.cabal-install hs.zlib ]));
  in
  pkgs.mkShell {
    buildInputs = [
      ghc
      pkgs.pkgconfig
      pkgs.zlib
    ];
  }
