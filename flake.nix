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
      # this appears to be broken due to a bug in nix
      # either that or i just do not understand
      # in particular if you comment out the nixpkgs.follows line, nix flake lock becomes unable to generate a lockfile
      # it works if you keep both lines but then the hls that gets produced doesnt work, and iirc it was equivalent to the hackage build one
      # either way im not really sure what's going on
      # there are multiple bugs open and closed on nix github wrt transitive dependencies so i feel like its likely it's just a bug
      # alternatively, nixpkgs might be fine, and it might just be https://github.com/input-output-hk/haskell.nix/issues/1961 or https://github.com/input-output-hk/haskell.nix/issues/1830

      #inputs = {
      #  nixpkgs.follows = "nixpkgs";
      #  haskell-nix.follows = "haskell-nix";
      #};

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
