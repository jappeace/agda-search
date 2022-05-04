{pkgs ? import ../pkgs.nix }:
let
  our-ghc = pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [
    shake directory tagsoup
    text containers uri-encode
    process aeson Agda pandoc SHA
  ]);
  shakefile = import "${sources}/support/nix/build-shake.nix"
    {
      inherit our-ghc;
      haskellPackages = pkgs.haskellPackages;
      inherit (pkgs) removeReferencesTo stdenv;
      name = "1lab-shake";
      main = "Main.hs";
    };

  default = import "${sources}";

  sources = builtins.fetchGit {
    url = "https://github.com/plt-amy/1lab";
    rev = "e7f3da6c3ef5054c791abb6e26eccf097c6b6a59";
  };

in
pkgs.runCommand "1lab.db" {} ''
    set -xe
    cp -R ${sources}/* ./
    chmod 700 -R ./*
    ${pkgs.haskellPackages.agda-search}/bin/agda-search ./ ./src/index.lagda.md $out --command createdb --cubical
  ''
