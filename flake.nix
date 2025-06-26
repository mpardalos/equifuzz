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

        devTools = [
          pkgs.haskellPackages.cabal-fmt
          pkgs.haskellPackages.cabal-install
          pkgs.haskellPackages.haskell-language-server
          pkgs.haskellPackages.hlint
          pkgs.haskellPackages.hoogle
        ];

        deps = [ pkgs.makeWrapper pkgs.clang pkgs.systemc ];

        packageModifiers = [
          ((t.flip hl.addBuildTools) deps)
          (hl.compose.overrideCabal (drv: {
            postInstall = ''
              # Patch equifuzz to know where systemc and clang are here
              echo 'postInstall: Wrapping binary'
              wrapProgram $out/bin/equifuzz \
                --set EQUIFUZZ_CLANG ${pkgs.clang}/bin/clang++ \
                --set SYSTEMC_HOME ${pkgs.systemc}
            '';
          }))

          hl.dontCheck
          hl.dontHaddock
          hl.enableStaticLibraries
          hl.justStaticExecutables
          hl.disableLibraryProfiling
          hl.disableExecutableProfiling
        ];

        shellModifiers = [
          ((t.flip hl.addBuildTools) (devTools ++ deps))
          (hl.compose.overrideCabal (drv: {
            shellHook = ''
              export SYSTEMC_HOME=${pkgs.systemc}
              export EQUIFUZZ_CLANG=${pkgs.clang}/bin/clang++
            '';
          }))
        ];

        equifuzz = { returnShellEnv ? false }:
          pkgs.haskellPackages.developPackage {
            root = ./.;
            name = "equifuzz";
            inherit returnShellEnv;

            modifier = t.flip t.pipe
              (if returnShellEnv then shellModifiers else packageModifiers);
          };
      in {
        defaultPackage = equifuzz { returnShellEnv = false; };
        devShell = equifuzz { returnShellEnv = true; };
      });
}
