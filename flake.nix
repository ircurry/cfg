#  .   .         .
#  |\  |        _|_
#  | \ | .-.  .-.|  .  . .--..--. .-.
#  |  \|(   )(   |  |  | |   |  |(.-'
#  '   ' `-'  `-'`-'`--`-'   '  `-`--'

{
  description = "Nixos config flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    firefox-addons = {
      url = "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    arkenfox = {
      url = "github:dwarfmaster/arkenfox-nixos";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    devenv.url = "github:cachix/devenv";

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs:
    let
      # ===Dealing with System===
      forSystem = function: system: function inputs.nixpkgs.legacyPackages.${system};

      systems = [ "x86_64-linux" ];

      forAllSystems = function: inputs.nixpkgs.lib.genAttrs systems (forSystem function);

      # ===My Library===
      mylib = import ./lib {
        inherit (inputs.nixpkgs) lib;
        inherit (inputs) nixpkgs;
      };

      # ===Common Attributes===
      commonAttrs = {
        inherit (inputs.nixpkgs) lib;
        inherit (inputs) nixpkgs self;
        inherit inputs mylib;
      };
    in
    {
      # ===NixOS Configurations===
      nixosConfigurations = import ./hosts commonAttrs;

      # ===Packages===
      packages = forAllSystems (pkgs: (import ./packages { inherit pkgs inputs; }));

      # ======Development Enviornment===
      devShells = forAllSystems (pkgs: {
        default = import ./devenv.nix {
          inherit inputs;
          inherit (pkgs) system;
        };
      });
    };
}
