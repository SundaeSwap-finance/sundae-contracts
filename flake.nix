{
  description = "SundaeSwap smart contracts";

  inputs = {
    nixpkgs = {
      follows = "haskell-nix/nixpkgs-unstable";
    };

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "haskell-nix/nixpkgs-2111";
    };

    plutus-flake-utils = {
      url = "github:SundaeSwap-finance/plutus-flake-utils/vasil";
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
        compiler-nix-name = "ghc8107";
        extraSha256map = {
          "https://github.com/chessai/wai-lambda.git"."f8b8dec8ab48f49d36810e6728a710ebb352efde" =
            "170vz513h6p61pwfwyw6ynbmn5c3gn8f4ln3ggv1l6i45hma5isb";
          "https://github.com/rrruko/aws-lambda-haskell-runtime.git"."8bdff2a3165012d2d548180ecaa679c35e45b572" =
            "0drgv8145846dqzdn7gcii3wvqfi72b303qk243fz44yvf3wx51y";
          "https://github.com/rrruko/aws-lambda-haskell-runtime-wai.git"."0e7c843ed997c3c29e16bc49b6db6b472c6902a7" =
            "1vlbcn1fvqyxpfgmdhakmmdbfrig3hxf4c53shw728lfhxnckq51";
        };
      };
    in
    flake-utils.lib.eachSystem supportedSystems (system: rec {
      pkgs = plutus-flake-utils.pkgs system;

      # we build everything unoptimised except for the docker image
      # which is only built on `main`
      inherit (plutus-flake-utils.plutusProject system (projectArgs false))
        project flake devShell;

      freezer-lambda = VERSION:
        let
          pkgs = self.pkgs.${system}.static;
          strip = drv: drv.overrideAttrs (old: {
            postInstall = old.postInstall or "" + ''
              for FILE in $out/bin/*; do
                if [[ -x $FILE ]]
                then
                  ${pkgs.bintools}/bin/strip $FILE
                fi
              done
            '';
          });
          freezerwriter = strip (flake.static.packages."lock:exe:freezer-writer");
        in
          pkgs.stdenv.mkDerivation {
            pname = "freezer-lambda";
            version = "0.1";
            src = ./freezer;
            phases = ["installPhase"];
            installPhase = ''
              mkdir -p $out
              sed "s/latest/${VERSION}/g" "$src/resources.template" > $out/resources.template
              ${pkgs.zip}/bin/zip -j $out/lambda.zip $out/resources.template $src/bootstrap $src/*.protocol.parameters ${freezerwriter}/bin/freezer-writer $src/*-script-bytes.json
            '';
          };
    });
}
