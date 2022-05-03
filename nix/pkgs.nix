import ./pin.nix {
  config = {

    packageOverrides = pkgs: {

        haskell = pkgs.lib.recursiveUpdate pkgs.haskell {
        packageOverrides = hpNew: hpOld: {
              agda-search = hpNew.callPackage ../agda-search {};
              agda-search-web = hpNew.callPackage ../agda-search-web {};
            };
        };
    };
  };
}
