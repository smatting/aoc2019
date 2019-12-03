#!/bin/sh
nix-shell shell.nix --run "hoogle server --local --port 8008"
