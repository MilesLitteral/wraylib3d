{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        all = flavour:
          pkgs.symlinkJoin {
            name = "ghc-wasm";
            paths = [
              pkgs.haskellPackages.alex
              pkgs.haskellPackages.happy
              (pkgs.callPackage ./pkgs/wasm32-wasi-ghc.nix { inherit flavour; })
              wasi-sdk
              deno
              nodejs
              bun
              binaryen
              wabt
              wasmtime
              wasmedge
              wizer
              cabal
              (pkgs.callPackage ./pkgs/wasm32-wasi-cabal.nix {
                inherit flavour;
              })
              proot
              wasm-run
            ];
          };
        wasm32-wasi-ghc-gmp =
          pkgs.callPackage ./pkgs/wasm32-wasi-ghc.nix { flavour = "gmp"; };
        wasm32-wasi-ghc-native =
          pkgs.callPackage ./pkgs/wasm32-wasi-ghc.nix { flavour = "native"; };
        wasm32-wasi-ghc-unreg =
          pkgs.callPackage ./pkgs/wasm32-wasi-ghc.nix { flavour = "unreg"; };
        wasm32-wasi-ghc-9_6 =
          pkgs.callPackage ./pkgs/wasm32-wasi-ghc.nix { flavour = "9.6"; };
        wasm32-wasi-cabal-gmp =
          pkgs.callPackage ./pkgs/wasm32-wasi-cabal.nix { flavour = "gmp"; };
        wasm32-wasi-cabal-native =
          pkgs.callPackage ./pkgs/wasm32-wasi-cabal.nix { flavour = "native"; };
        wasm32-wasi-cabal-unreg =
          pkgs.callPackage ./pkgs/wasm32-wasi-cabal.nix { flavour = "unreg"; };
        wasm32-wasi-cabal-9_6 =
          pkgs.callPackage ./pkgs/wasm32-wasi-cabal.nix { flavour = "9.6"; };
        wasi-sdk = pkgs.callPackage ./pkgs/wasi-sdk.nix { };
        deno = pkgs.callPackage ./pkgs/deno.nix { };
        nodejs = pkgs.callPackage ./pkgs/nodejs.nix { };
        bun = pkgs.callPackage ./pkgs/bun.nix { };
        binaryen = pkgs.callPackage ./pkgs/binaryen.nix { };
        wabt = pkgs.callPackage ./pkgs/wabt.nix { };
        wasmtime = pkgs.callPackage ./pkgs/wasmtime.nix { };
        wasmedge = pkgs.callPackage ./pkgs/wasmedge.nix { };
        wazero = pkgs.callPackage ./pkgs/wazero.nix { };
        wizer = pkgs.callPackage ./pkgs/wizer.nix { };
        cabal = pkgs.callPackage ./pkgs/cabal.nix { };
        proot = pkgs.callPackage ./pkgs/proot.nix { };
        wasm-run = pkgs.callPackage ./pkgs/wasm-run.nix { };
      in
      {
        packages = {
          inherit all wasm32-wasi-ghc-gmp wasm32-wasi-ghc-native
            wasm32-wasi-ghc-unreg wasm32-wasi-ghc-9_6 wasm32-wasi-cabal-gmp
            wasm32-wasi-cabal-native wasm32-wasi-cabal-unreg
            wasm32-wasi-cabal-9_6 wasi-sdk deno nodejs bun binaryen wabt
            wasmtime wasmedge wazero wizer cabal proot wasm-run;
          default = all "gmp";
          all_gmp = all "gmp";
          all_native = all "native";
          all_unreg = all "unreg";
          all_9_6 = all "9.6";
        };
      });
}
