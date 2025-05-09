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
        default = import ./shell.nix { inherit pkgs; };
      });

      # ===Templates===
      templates = import ./templates;
    };
}
