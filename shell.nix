{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc801" }:

let

  inherit (nixpkgs) pkgs;
  inherit (pkgs.lib) inNixShell;

  f = import ./default.nix;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = pkgs.haskell.lib.addBuildTools (haskellPackages.callPackage f {})
    (with haskellPackages; [ hlint cabal-install ]);

in

  if pkgs.lib.inNixShell then drv.env else drv
