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
  unstablepkgs = import <nixos-unstable> { inherit config; };
  packages = pkgs.haskell.packages;
  haskell = packages.${compiler};
  env = haskell.project.env.overrideAttrs (
    old: with haskell; {
      nativeBuildInputs = old.nativeBuildInputs ++ [
        hlint
        ormolu
        brittany
        unstablepkgs.haskell.packages.${compiler}.ghcide
        cabal-install
      ];
    }
  );

in
{
  haskell = haskell;
  project = haskell.project;
  env = env;
  }
