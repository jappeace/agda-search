{ pkgs ? import <nixpkgs> { }, ... }:
pkgs.mkShell {
  packages = [ pkgs.cabal-install ];
  inputsFrom = [ (pkgs.haskellPackages.callCabal2nix "agda-search" ./. { }).env ];
}
