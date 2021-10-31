# { pkgs ? import <nixpkgs> {}, compiler ? "ghc901" }:
# pkgs.haskell.lib.buildStackProject {
#   inherit compiler;
#   name = "server";
#   buildInputs = with pkgs; [postgresql lzma.dev git zlib imagemagick glibc];
# }
# {ghc ? "ghc901" }:
# let pkgs = import <nixpkgs> {};
# in  { server = pkgs.haskell.lib.buildStackProject {
#       inherit ghc;
#       name = "server";
#       buildInputs = with pkgs; [postgresql lzma.dev git zlib imagemagick glibc]; };
#    }

{ghc}:
with (import <nixpkgs> {});
haskell.lib.buildStackProject {
  inherit ghc;
  name = "myEnv";
  buildInputs = [ postgresql lzma.dev git zlib imagemagick ]; }
