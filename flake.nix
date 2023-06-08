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
      url = "github:SundaeSwap-finance/plutus-flake-utils/rrruko/chap";

      # try to reduce duplicate builds
      inputs = {
        nixpkgs.follows = "nixpkgs";
        haskell-nix.follows = "haskell-nix";
      };

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
        compiler-nix-name = "ghc927";
      };
    in
    flake-utils.lib.eachSystem supportedSystems (system: 
      let
        plutusProject = (plutus-flake-utils.plutusProject system (projectArgs false));
      in rec {
        pkgs = plutus-flake-utils.pkgs system;
        inherit (plutusProject) flake project devShell;

        #devShell = plutusProject.devShell.overrideAttrs (old: {
        #  nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.static.haskellPackages.cabal-install pkgs.static.haskellPackages.haskell-language-server ];
        #});
      }
    );
}
