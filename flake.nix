{
  description = "Haskell MOOC development environment with Cabal support";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
  };

  outputs =
    { nixpkgs, ... }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      forEachSupportedSystem =
        f:
        nixpkgs.lib.genAttrs supportedSystems (
          system:
          f {
            inherit system;
            pkgs = import nixpkgs { inherit system; };
          }
        );
      ghcVersion = "ghc928";
    in
    {
      devShells = forEachSupportedSystem (
        { pkgs, system }:
        let
          haskellPackages = pkgs.haskell.packages.${ghcVersion};
          cabal-build = pkgs.writeShellApplication {
            name = "cabal-build";
            runtimeInputs = with pkgs; [
              cabal-install
              haskellPackages.ghc
              git
            ];
            text = ''
              cd "$(git rev-parse --show-toplevel)/exercises"
              cabal v2-build
            '';
          };

          cabal-test = pkgs.writeShellApplication {
            name = "cabal-test";
            runtimeInputs = with pkgs; [
              cabal-install
              haskellPackages.ghc
              git
              coreutils
            ];
            text = ''
              file=$(realpath "$1")
              cd "$(git rev-parse --show-toplevel)/exercises"
              cabal v2-exec runhaskell "$file"
            '';
          };

        in
        {
          default = pkgs.mkShell {
            packages = with pkgs; [
              cabal-install
              haskellPackages.ghc
              haskellPackages.haskell-language-server
              zlib
              pkg-config
              cabal-build
              cabal-test
            ];

            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.zlib ];
          };
        }
      );
    };
}
