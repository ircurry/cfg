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
    nixpkgs-waybar.url = "github:nixos/nixpkgs/cead77df2557634c834691272bb19a0c3930e939";

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
      url = "github:nix-community/emacs-overlay/a3bf20522b1fa7295ca05ea4a8d37546915e598e";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    dfh = {
      url = "github:ircurry/dfh";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nocturne-tools = {
      url = "github:ircurry/nocturne-tools";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    niri = {
      url = "github:sodiboo/niri-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ags = {
      url = "github:aylur/ags";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { nixpkgs, self, ... }@inputs:
    let
      # ===Nixpkgs Lib===
      inherit (nixpkgs) lib;

      # ===Dealing with System===
      forSystem = function: system: function nixpkgs.legacyPackages.${system};

      systems = [ "x86_64-linux" ];

      forAllSystems = function: lib.genAttrs systems (forSystem function);

      # ===My Library===
      mylib = import ./lib {
        inherit nixpkgs lib;
      };

      # ===Common Attributes===
      commonAttrs = {
        inherit
          inputs
          mylib
          nixpkgs
          lib
          self
          ;
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
        default = import ./shell.nix {
          inherit pkgs;
          ags = inputs.ags.packages.${pkgs.system}.agsFull;
        };
      });

      # ===Templates===
      templates = import ./templates;
    };
}
