{
  description = "Advent of Code 2020.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        haskellPackages = pkgs.haskellPackages;
      in
      {
        devShells.default = pkgs.mkShell {
          name = "AOC 2020 dev shell";

          buildInputs = with haskellPackages; [
            ghc
            cabal-install
            haskell-language-server
            hlint
            ormolu
          ];

          shellHook = ''
            echo "GHC version: $(ghc --version)"
          '';
        };
      }
    );
}
