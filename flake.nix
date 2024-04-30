#  .   .         .
#  |\  |        _|_
#  | \ | .-.  .-.|  .  . .--..--. .-.
#  |  \|(   )(   |  |  | |   |  |(.-'
#  '   ' `-'  `-'`-'`--`-'   '  `-`--'

{
  description = "Nixos config flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    #nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";

    hyprland = {
      #url = "github:hyprwm/Hyprland/";
      url = "github:hyprwm/Hyprland/4540d8ccd51e485485af364a951ea7df240585be";
    };

    nh = {
      url = "github:viperML/nh";
      inputs.nixpkgs.follows = "nixpkgs";
    };

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

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    devenv = {
      url = "github:cachix/devenv";
      inputs.nixpkgs.follows = "nixpkgs";
    };

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

      # ===My Library Exported===
      lib = mylib;

      # ===Packages===
      packages = forAllSystems (pkgs: (import ./packages { inherit pkgs inputs mylib; }));

      # ===Development Enviornment===
      devShells = forAllSystems (pkgs: {
        default = import ./devenv.nix {
          inherit inputs;
          inherit (pkgs) system;
        };
      });

      # ===Templates===
      templates = import ./templates;
    };
}
