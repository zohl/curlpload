{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  haskellPackages_ = haskellPackages.override {
    overrides = self: super: {
      postgresql-simple-bind = self.callPackage ~/src/haskell/postgresql-simple-bind/default.nix {};
      warp-autoquit = self.callPackage ~/src/haskell/warp-autoquit/default.nix {};
      warp-socket-activation = self.callPackage ~/src/haskell/warp-socket-activation/default.nix {};
    };
  };

  drv = haskellPackages_.callPackage ./default.nix {};

in 
  if pkgs.lib.inNixShell then drv.env else drv
