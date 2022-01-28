{
  description = "Curl Runnings Nix Flake";
  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-2111"; # FLAKE REF
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
          overlays = [
            haskellNix.overlay
            (final: prev: {
              curlRunnings =
                final.haskell-nix.project' {
                  src = ./.;
                  compiler-nix-name = "ghc8107";
                  shell.tools =
                    let
                      idx = { index-state = "2022-01-01T00:00:00Z"; };
                    in
                    {
                      cabal-install = idx;
                      cabal-plan = idx;
                      ghcid = idx;
                      haskell-language-server = idx;
                      hlint = idx;
                      hoogle = idx;
                      hpack = idx;
                      ormolu = { version = "0.3.1.0"; } // idx;
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
        flake // {
          defaultPackage = flake.packages."curl-runnings:exe:curl-runnings";
        }
      );
}
