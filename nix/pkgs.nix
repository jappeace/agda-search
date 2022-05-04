import ./pin.nix {
  config = {

    packageOverrides = pkgs: {

        haskell = pkgs.lib.recursiveUpdate pkgs.haskell {
        packageOverrides = hpNew: hpOld: {
              agda-search = hpNew.callPackage ../agda-search {};
              agda-search-web = hpNew.callPackage ../agda-search-web {};
    agda-fold-equations = pkgs.haskellPackages.callCabal2nix
      "agda-fold-equations"
      (fetchGit {
        url = "https://git.amelia.how/amelia/agda-fold-equations.git";
        rev = "509bc021200b0de7713a0fdb27c730eaff3be206";
        ref = "main";
      }) {};

    agda-reference-filter = pkgs.haskellPackages.callCabal2nix
      "agda-fold-equations"
      (fetchGit {
        url = "https://git.amelia.how/amelia/agda-reference-filter.git";
        rev = "082b5576e799fe8aa28e7d09cf415ac6c9e0596b";
        ref = "master";
      }) {};

    Agda = pkgs.haskell.lib.overrideCabal hpOld.Agda {
      version = "2.6.2.1";
      sha256 = sha256:03dw7jfqr3ffik6avigm525djqh2gn5c3qwnb2h6298zkr9lch9w;
    };
            };
        };
    };
  };
}
