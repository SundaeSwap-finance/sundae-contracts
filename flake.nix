{
  description = "SundaeSwap smart contracts";

  inputs = {
    nixpkgs = {
      follows = "haskell-nix/nixpkgs-unstable";
    };

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    };

    flake-utils = {
      url = "github:numtide/flake-utils";
    };

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      flake = false;
    };

    chap = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };

  outputs =
    { self
    , nixpkgs
    , haskell-nix
    , iohk-nix
    , flake-utils
    , chap
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
        /*
        extraSha256map = {
          "https://github.com/chessai/wai-lambda.git"."f8b8dec8ab48f49d36810e6728a710ebb352efde" =
            "170vz513h6p61pwfwyw6ynbmn5c3gn8f4ln3ggv1l6i45hma5isb";
          "https://github.com/rrruko/aws-lambda-haskell-runtime.git"."8bdff2a3165012d2d548180ecaa679c35e45b572" =
            "0drgv8145846dqzdn7gcii3wvqfi72b303qk243fz44yvf3wx51y";
          "https://github.com/rrruko/aws-lambda-haskell-runtime-wai.git"."0e7c843ed997c3c29e16bc49b6db6b472c6902a7" =
            "1vlbcn1fvqyxpfgmdhakmmdbfrig3hxf4c53shw728lfhxnckq51";
            };
            */
      };
      config = haskell-nix.config;
      overlaysFor = doStatic:
        let
          lzmaStaticOverlay = self: super: {
            lzma = super.lzma.overrideAttrs (old: {
              dontDisableStatic = true;
            });
          };
        in
        [ haskell-nix.overlay ]
        ++ (import iohk-nix {}).overlays.crypto
        ++ nixpkgs.lib.optional doStatic lzmaStaticOverlay;
      nixpkgsFor = doStatic: system:
      import nixpkgs {
        inherit system;
        inherit config;
        overlays = overlaysFor doStatic;
      };
      projectFor = doStatic: system:
      { packages ? throw "packages must be provided"
      , src ? throw "src must be provided"
      , compiler-nix-name ? throw "compiler-nix-name must be provided"
      }:
      let
        linker-workaround = pkgs.writeShellScript "linker-workaround"
          (import ./linker-workaround.nix pkgs.stdenv);
        gitignore = pkgs.nix-gitignore.gitignoreSourcePure
          (import ./default-gitignore.nix);
        atLeastGHC9 = builtins.match "ghc9" compiler-nix-name != null;
        defaultGHCOptions = [
            "-Wall"
            "-Wcompat" # note: in the future, this will be folded into -Wall.
            "-Wcpp-undef"
            "-Widentities"
            "-Wincomplete-record-updates"
            "-Wincomplete-uni-patterns"
            "-Wmissing-deriving-strategies"
            "-Wmissing-export-lists"
            "-Wpartial-fields"
        ] ++ nixpkgs.lib.optional atLeastGHC9 [ "-Winvalid-haddock" ];
        setGHCOptions = ps: nixpkgs.lib.foldr
          (p: acc: {
            "${p}".ghcOptions =
              nixpkgs.lib.optional doStatic "-pgml=${linker-workaround}"
              ++ defaultGHCOptions;
          })
          {}
          ps;
        deferPluginErrors = true;
        pkgs =
          let
            basePkgs = nixpkgsFor doStatic system;
          in
          if doStatic
          then basePkgs.pkgsCross.musl64
          else basePkgs;
      in
      pkgs.haskell-nix.cabalProject' {
        inherit src;
        inherit compiler-nix-name;
        cabalProjectFileName = "cabal.project";
        modules = [{
          packages = {
            marlowe.flags.defer-plugin-errors = deferPluginErrors;
            plutus-use-cases.flags.defer-plugin-errors = deferPluginErrors;
            plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;
            plutus-contract.flags.defer-plugin-errors = deferPluginErrors;
            cardano-crypto-praos.components.library.pkgconfig =
              nixpkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
            cardano-crypto-class.components.library.pkgconfig =
              nixpkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
            beam-migrate.flags.werror = false;
          } //
          (setGHCOptions packages);
        }];
        shell = {
          packages = ps: builtins.map (p: ps."${p}") packages;
          withHoogle = true;
          exactDeps = true;
          buildInputs = [
            pkgs.cabal-install
            pkgs.ghcid
            pkgs.xxd
          ];
        };
        inputMap = {
          "https://input-output-hk.github.io/cardano-haskell-packages" = chap;
        };
      };
      pkgs = system: {
        static = nixpkgsFor true system;
        dynamic = nixpkgsFor false system;
      };
      plutusProject = system: args: rec {
        project = {
          static = projectFor true system args;
          dynamic = projectFor false system args;
        };

        flake = {
          static = project.static.flake {};
          dynamic = project.dynamic.flake {};
        };

        devShell = flake.dynamic.devShell;
      };
    in
    flake-utils.lib.eachSystem supportedSystems (system: rec {
      inherit (plutusProject system (projectArgs false))
        project flake devShell;
    });
}
