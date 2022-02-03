{
  description = "Curl Runnings Nix Flake (Stack)";
  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs"; # FLAKE REF
    flake-utils.url = github:numtide/flake-utils;
    haskellNix = {
      url = github:input-output-hk/haskell.nix;
      inputs.nixpkgs.follows = "haskellNix/nixpkgs-2111"; # FLAKE REF
    };
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "aarch64-darwin" "x86_64-darwin" "x86_64-linux" ]
      (system:
        let
          idx = { index-state = "2022-02-01T00:00:00Z"; };
          overlays = [
            haskellNix.overlay
            (final: prev: {
              curlRunnings =
                final.haskell-nix.stackProject' {
                  src = ../.;
                  shell.tools = {
                    cabal-install = idx;
                    hpack = idx;
                  };
                };
            })
          ];
          pkgs = import nixpkgs {
            inherit system overlays;
            inherit (haskellNix) config;
          };
          flake = pkgs.curlRunnings.flake { };
        in
        flake // rec {
          defaultPackage = flake.packages."curl-runnings:exe:curl-runnings";
        }
      );
}
