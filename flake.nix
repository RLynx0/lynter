{
  description = "A repl and interpreter for a lambda-calculus-esque language";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages."x86_64-linux";
    in
    {

      devShells."x86_64-linux".default = pkgs.mkShell {
        buildInputs = with pkgs; [
          cargo
          rustc

          clippy
          rust-analyzer
          rustfmt
        ];
        env.RUST_SRC_PATH = "${pkgs.rust.packages.stable.rustPlatform.rustLibSrc}";
      };

    };
}
