{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { nixpkgs, ... }:
    let
      system = "aarch64-darwin";
      pkgs = import nixpkgs { inherit system; };
    in {
      devShells.${system}.default = pkgs.mkShell {
        nativeBuildInputs = [
          pkgs.pkg-config
        ];

        buildInputs = [
          pkgs.poppler
          pkgs.glib
          pkgs.cairo
          pkgs.libpng
          pkgs.zlib
        ];
      };
    };
}
