{
  db = import ./create-db.nix {};
  server = import ../agda-search-web {};
  agda-sources = (import ./pkgs.nix).agdaPackages.standard-library;
}
