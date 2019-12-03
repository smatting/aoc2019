#!/bin/sh
nix-shell shell-nixpkgs.nix --run "cabal new-configure"  
plan-to-nix -o haskell.nix
