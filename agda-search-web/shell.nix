{ pkgs ? import ../nix/pkgs.nix, ... }:
pkgs.haskellPackages.shellFor {
  packages = ps : [ ps.agda-search-web ];
  buildInputs = [
        pkgs.cabal-install
        ];
}
