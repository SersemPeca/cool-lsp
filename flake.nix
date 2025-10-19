{
  description = "Rust flake";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable"; # or whatever vers
  };

  outputs =
    { self, nixpkgs, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        packages = with pkgs; [
          # Rust
          rustc
          rustfmt
          cargo

          # Testing
          python3
        ];

        shellHook = ''
          exec fish
        '';
      };
    };
}
