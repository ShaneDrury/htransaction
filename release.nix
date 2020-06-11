{ compiler ? "ghc865" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              project =
                haskellPackagesNew.callPackage ./project.nix { };

              co-log-polysemy =
                haskellPackagesNew.callPackage ./nix/co-log-polysemy.nix {};
            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { project = pkgs.haskell.packages.${compiler}.project;
  }
