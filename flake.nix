{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    inputs:
    inputs.flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        hp = pkgs.haskellPackages;
        drv =
          {
            mkDerivation,
            base,
            megaparsec,
            parser-combinators,
            process,
            relude,
          }:
          mkDerivation {
            pname = "bask";
            version = "0.1.0.0";
            src = ./.;
            isLibrary = false;
            isExecutable = true;
            executableHaskellDepends = [
              base
              megaparsec
              parser-combinators
              process
              relude
            ];
            description = "A framework to glue CLI commands together.";
            license = "mit";
          };
        bask = pkgs.haskellPackages.callPackage drv { };
      in
      {
        packages = {
          default = bask;
          inherit bask;
        };
        devShells = {
          default = pkgs.mkShell {
            name = "bask";
            buildInputs = [
              bask
              hp.cabal-install
              hp.ghc
              hp.ormolu
              hp.cabal-fmt
              hp.hoogle
            ];
          };
        };
      }
    );
}
