{
  description = "Fuzzer for equivalence checkers";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        t = pkgs.lib.trivial;
        hl = pkgs.haskell.lib;

        project = devTools: # [1]
          let addBuildTools = (t.flip hl.addBuildTools) devTools;
          in pkgs.haskellPackages.developPackage {
            root = ./.;
            name = "equifuzz";
            returnShellEnv = !(devTools == [ ]); # [2]
            
            modifier = (t.flip t.pipe) [
              addBuildTools
              hl.dontCheck
              hl.dontHaddock
              hl.enableStaticLibraries
              hl.justStaticExecutables
              hl.disableLibraryProfiling
              hl.disableExecutableProfiling
            ];
          };

      in {
        packages.pkg = project [ ]; # [3]

        defaultPackage = self.packages.${system}.pkg;

        devShell = project (with pkgs.haskellPackages; [ # [4]
          cabal-fmt
          cabal-install
          haskell-language-server
          hlint
          pkgs.clang 
          pkgs.systemc 
        ]);
      }); 
}
