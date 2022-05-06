{
  stdlib = import nix/db/stdlib.nix {};
  _1lab = import nix/db/1lab.nix {};
  server = import ./agda-search-web {};
  search = import ./agda-search {};
  agda-sources = (import nix/pkgs.nix).agdaPackages.standard-library;
}
