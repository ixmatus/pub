let config = import ./config.nix;
in
{ pkgs ? import <nixpkgs> { inherit config; } }:
let
  darwinPkgs = import <nixpkgs> { inherit config; system = "x86_64-darwin"; };
  linuxPkgs  = import <nixpkgs> { inherit config; system = "x86_64-linux" ; };
  pkgs       = import <nixpkgs> { inherit config; };

in
  { pub-linux  =  linuxPkgs.haskellPackages.pub;
    pub-darwin = darwinPkgs.haskellPackages.pub;
    pub        =       pkgs.haskellPackages.pub;
  }
