{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:
pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "server";
  buildInputs = with pkgs; [postgresql lzma.dev git zlib imagemagick];
}