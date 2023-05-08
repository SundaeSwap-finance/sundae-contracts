{
  description = "SundaeSwap smart contracts";

  inputs = {
    nixpkgs = {
      follows = "haskell-nix/nixpkgs-unstable";
    };

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
    };

    plutus-flake-utils = {
      url = "/home/bukulon/lam/plutus-flake-utils";
    };

    flake-utils = {
      url = "github:numtide/flake-utils";
    };

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

  };

  outputs =
    { self
    , nixpkgs
    , haskell-nix
    , plutus-flake-utils
    , flake-utils
    , ...
    }:
    let
      # can be extended if we ever have anyone on MacOS or need to cross compile.
      # systems outside of this list have not been tested
      supportedSystems =
        [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];

      projectArgs = isDocker: {
        packages = [
          "onchain"
        ];
        src = ./.;
        compiler-nix-name = "ghc8107";
        extraSha256map = {
          "https://github.com/input-output-hk/cardano-base.git"."07d40428a55b0a80ec49126f09d8e4d8191578b3" = "0g22xhvil88f1559rhayd3c76iyfx2c5l6pgy3wank22zp70ij2q";
        };
      };
    in
    flake-utils.lib.eachSystem supportedSystems (system: rec {
      pkgs = plutus-flake-utils.pkgs system;
      inherit (plutus-flake-utils.plutusProject system (projectArgs false))
        project flake devShell;
    });
}
