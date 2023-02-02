{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = { self, nixpkgs }:
    let pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in {
      devShell.x86_64-linux = pkgs.mkShell {
        buildInputs = with pkgs; [ haskell.compiler.ghc924 pkgs.haskellPackages.haskell-language-server cabal-install yosys symbiyosys verilog ];
      };
    };
}
