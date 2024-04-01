{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", profiling ? false }:

let

  inherit (nixpkgs) pkgs;

  f = import ./hpython.nix;

  haskellPackages =
    ((if compiler == "default"
      then pkgs.haskellPackages
      else pkgs.haskell.packages.${compiler})).override {
       overrides = self: super: {
         mkDerivation = expr:
           super.mkDerivation (expr // { enableLibraryProfiling = profiling; });
         digit = self.callPackage ./nix/digit.nix {};
         parsers-megaparsec = self.callPackage ./nix/parsers-megaparsec.nix {};
         validation = self.callPackage ./nix/validation.nix {};
	 megaparsec = super.megaparsec_6_5_0;
	 tasty = super.callHackage "tasty" "1.1.0.4" {};
       };
     };

  drv = haskellPackages.callPackage f {};

in

  drv
