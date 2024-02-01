{
  description = "A description of your project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    aiken.url = "github:aiken-lang/aiken/v1.0.20-alpha";
    # wont work on macos till they cut a release with the coreservices stuff
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, aiken, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ aiken.outputs.overlays.${system}.default ];
        };
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [ pkgs.aiken ];
        };
      }
    );
}
