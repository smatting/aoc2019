let
  hsPkgs = import ./haskell.nix/default-pinned.nix {};
in
hsPkgs.shellFor {
    packages = ps: [ps.aoc2019];
    withHoogle = true;
    shellHook = ''
      export NIX_GHC_LIBDIR="$(dirname $(which ghc))/../lib/ghc-8.6.5";
    '';
}
