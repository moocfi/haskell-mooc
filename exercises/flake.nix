{
  description = "Haskell MOOC: Cabal Edition";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs =
    { flake-parts
    , ...
    }@inputs: flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];

      perSystem = { system, pkgs, ... }:
        let
          hlib = pkgs.haskell.lib;
          hpkgs = pkgs.haskell.packages."ghc912".override {
            overrides = self: super: {
              zlib = hlib.dontCheck (hlib.doJailbreak super.zlib);
            };
          };

          tests = pkgs.haskell.lib.overrideCabal (hpkgs.callCabal2nix "tests" ./. { }) (_: {
            doCheck = true;
            doHaddock = false;
            enableLibraryProfiling = false;
            enableExecutableProfiling = false;
          });
        in
        {
          packages.default = tests;

          devShells.default = pkgs.mkShell {
            buildInputs = [
              hpkgs.cabal-install
              hpkgs.haskell-language-server
              hpkgs.fourmolu
              hpkgs.ghcid
              hpkgs.ghc
              pkgs.haskellPackages.cabal-fmt
              pkgs.haskellPackages.implicit-hie
              pkgs.zlib
              pkgs.zlib.dev
            ];
          };
        };
    };
}
