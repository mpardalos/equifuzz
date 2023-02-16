{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, brick, containers, hedgehog, lib
      , optics, recursion-schemes, shelly
      , string-interpolate, text, uniplate, uuid, vty
      }:
      mkDerivation {
        pname = "equifuzz";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        enableSharedExecutables = false;
        enableSharedLibraries = false;
        configureFlags = [
          "--ghc-option=-optl=-static"
          "--ghc-option=-optl=-pthread"
          "--ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
          "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
          "--ghc-option=-optl=-L${pkgs.glibc.static}/lib"
        ];
        executableHaskellDepends = [
          base brick containers hedgehog optics
          recursion-schemes shelly string-interpolate text uniplate uuid vty (import ./libverismith)
        ];
        license = lib.licenses.gpl3Only;
        mainProgram = "equifuzz";
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
