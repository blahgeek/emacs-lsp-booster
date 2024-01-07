{
  inputs = {
    nixpkgs.url     = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows     = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = { nixpkgs, rust-overlay, flake-utils, ... }:
    let overlay = final: prev: { emacs-lsp-booster = final.callPackage ./default.nix { }; };
    in {
      overlays.default = overlay;
    } // flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs {
            inherit system;
            overlays = [ (import rust-overlay) overlay ];
          };
      in with pkgs; {
        packages.default = pkgs.emacs-lsp-booster;

        devShells.default = mkShell {
          buildInputs = [
            rust-bin.stable.latest.default
            rust-analyzer
          ];
        };
      }
    );
}
