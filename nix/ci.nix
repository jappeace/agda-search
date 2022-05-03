{
  db = import ./create-db.nix {};
  server = import ../agda-search-web {};
}
