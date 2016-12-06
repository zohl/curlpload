{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  haskellPackages_ = haskellPackages.override {
    overrides = self: super: {
      postgresql-simple-bind = self.callPackage ../haskell/postgresql-simple-bind/default.nix {};
      warp-autoquit = self.callPackage ../haskell/warp-autoquit/default.nix {};
      warp-socket-activation = self.callPackage ../haskell/warp-socket-activation/default.nix {};
    };
  };

  drv = haskellPackages_.callPackage ./default.nix {};

in 
  if pkgs.lib.inNixShell then drv.env else pkgs.stdenv.mkDerivation {
    name = "${drv.pname}-package-${drv.version}";
    src = ./.;

    buildPhases = [ "installPhase" ];
    installPhase = ''
      mkdir -p "$out/share/${drv.pname}"
      cp -R ${drv}/* "$out/"
      cp -R "$src/db" "$out/share/${drv.pname}"
    '';
  }
