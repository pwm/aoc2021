{ pkgs, compiler }:
let
  lib = pkgs.lib;

  util = import ./util.nix {
    inherit pkgs;
    inherit (pkgs) lib gitignoreFilter;
  };

  conf = lib.importTOML ../nixkell.toml;

  ghcVersion = if compiler != null then compiler else conf.ghc;

  # Create our own setup using our choosen GHC version as a starting point
  ourHaskell = pkgs.haskell.packages.${("ghc" + util.removeDot ghcVersion)}.override {
    overrides =
      let
        depsFromDir = pkgs.haskell.lib.packagesFromDirectory {
          directory = ./packages;
        };
        manual = _hfinal: hprev: {
          aoc2021 =
            let
              filteredSrc = util.filterSrc ../. {
                ignoreFiles = conf.ignore.files;
                ignorePaths = conf.ignore.paths;
              };
            in
            hprev.callCabal2nix "aoc2021" filteredSrc { };
        };
      in
      lib.composeExtensions depsFromDir manual;
  };

  # Add our package with its dependencies to GHC
  ghc = ourHaskell.ghc.withPackages (_ps:
    pkgs.haskell.lib.getHaskellBuildInputs ourHaskell.aoc2021
  );

  tools = util.getFromPkgs conf.env.tools;

  scripts = import ./scripts.nix { inherit pkgs; };
in
{
  bin = util.leanPkg ourHaskell.aoc2021;

  shell = pkgs.buildEnv {
    name = "aoc2021-env";
    paths = [ ghc ] ++ tools ++ scripts;
  };
}
