{
  description = "horizon-platform-template";

  nixConfig = {
    extra-substituters = "https://horizon.cachix.org";
    extra-trusted-public-keys = "horizon.cachix.org-1:MeEEDRhRZTgv/FFGCv3479/dmJDfJ82G6kfUDxMSAw0=";
  };

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    horizon-devtools.url = "git+https://gitlab.horizon-haskell.net/package-sets/horizon-platform?ref=lts/ghc-9.8.x";
    horizon-platform.url = "git+https://gitlab.horizon-haskell.net/package-sets/horizon-platform?ref=lts/ghc-9.8.x";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "aarch64-darwin" "aarch64-linux" "x86_64-darwin" "x86_64-linux" ];
      perSystem = { pkgs, system, ... }:
        let
          myOverlay = final: prev: {
            psql-migrate = final.callCabal2nix "psql-migrate" ./. { };
          };
          legacyPackages = inputs.horizon-platform.legacyPackages.${system}.extend myOverlay;
        in
        rec {

          devShells.default = legacyPackages.shellFor {
            packages = p: [ p.psql-migrate ];
            buildInputs = [
              legacyPackages.cabal-install
            ];
          };

          inherit legacyPackages;

          packages = rec {
            inherit (legacyPackages)
              psql-migrate;
            default = packages.psql-migrate;
          };

        };
    };
}
