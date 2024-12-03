{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, rust-overlay, ... }:
    let

      system = "x86_64-linux";
      overlays = [ (import rust-overlay) ];
      pkgs = import nixpkgs { inherit system overlays; };
      rust = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
      run = pkgs.writeScriptBin "run"
        "elm-live src/Main.elm -- --output=www/elm.js --debug & chromium --app=http://localhost:8000";
      genWasm = pkgs.writeScriptBin "genWasm" ''
        cd wasm
        cargo build --target=wasm32-unknown-unknown --release
        wasm-bindgen --target=web target/wasm32-unknown-unknown/release/wasm.wasm --out-dir ../www
      '';
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [
          # pkgs.chromium
          run
          genWasm
          pkgs.lld
          pkgs.wasm-bindgen-cli
          pkgs.taplo
          pkgs.rust-analyzer
          pkgs.rustfmt
          rust
        ] ++
        (with pkgs.elmPackages; [ elm elm-live elm-language-server elm-format ]);
      };
    };
}
