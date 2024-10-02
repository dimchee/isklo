{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = { nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
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
        buildInputs = [ run genWasm pkgs.cargo pkgs.lld pkgs.wasm-bindgen-cli pkgs.taplo pkgs.rust-analyzer] ++
          (with pkgs.elmPackages; [ elm elm-live elm-language-server elm-format ]);
      };
    };
}
