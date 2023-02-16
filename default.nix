let
 pkgs = import <nixpkgs> { };
in
 pkgs.haskellPackages.callPackage ./equifuzz.nix { }
