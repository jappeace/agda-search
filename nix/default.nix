{
  stdlib = import ./db/stdlib.nix {};
  _1lab = import ./db/1lab.nix {};
  server = import ../agda-search-web {};
  agda-sources = (import ./pkgs.nix).agdaPackages.standard-library;
}
