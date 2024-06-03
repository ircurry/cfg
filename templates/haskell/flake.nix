{
  description = "Minimal flake for Haskell development";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
    };

    devenv = {
      url = "github:cachix/devenv";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{ flake-parts, nixpkgs, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ inputs.devenv.flakeModule ];
      systems = nixpkgs.lib.systems.flakeExposed;

      perSystem =
        {
          config,
          self',
          inputs',
          pkgs,
          system,
          ...
        }:
        {
          devenv.shells.default = {
            packages = with pkgs; [
              just
              ghcid
              hlint
            ];
            languages.nix.enable = true;
            languages.haskell.enable = true;
            languages.haskell.package = pkgs.haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [ ]);
          };
        };
    };
}
