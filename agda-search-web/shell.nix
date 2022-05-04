{ pkgs ? import ../nix/pkgs.nix, ... }:
pkgs.haskellPackages.shellFor {
  packages = ps : [ ps.agda-search-web ];
  buildInputs = [
        pkgs.cabal-install
        ];
  AGDA_SOURCES = pkgs.agdaPackages.standard-library;
  DB_PATH_STD_LIB = import ../nix/db/stdlib.nix {};
  DB_PATH_1LAB = import ../nix/db/1lab.nix {};
}
