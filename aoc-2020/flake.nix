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
            cabal-install
            ghc
            haskell-language-server
            hlint
            ormolu
            regex-base
            regex-posix
          ];

          shellHook = ''
            echo "GHC version: $(ghc --version)"
          '';
        };
      }
    );
}
