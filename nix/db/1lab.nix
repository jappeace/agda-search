{pkgs ? import ../pkgs.nix }:
let
  sources = builtins.fetchGit {
    url = "https://github.com/plt-amy/1lab";
    rev = "82394aebd120212bf0e21e441f3e2ed6318d1278";
  };

in
pkgs.runCommand "search-index.db" {} ''
    set -xe
    cp -R ${sources}/src/* ./
    chmod 700 -R ./*
    "${pkgs.haskellPackages.agda-search}"/bin/agda-search ./ ./index.lagda.md $out --command createdb --cubical
  ''
