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
  
  outputs = { ... }@inputs:
    let
      # ===Dealing with System===
      forSystem = function: system: function inputs.nixpkgs.legacyPackages.${system};

      systems = ["x86_64-linux"];

      forAllSystems = function:
        inputs.nixpkgs.lib.genAttrs systems (forSystem function);

      # ===NixOS Build Functions===
      mkSystemCustomModules = modules: config: inputs.nixpkgs.lib.nixosSystem {
        specialArgs = {inherit inputs;};
        modules = [
          config
        ] ++ modules;
      };
      
      commonNixosModules = [
        inputs.home-manager.nixosModules.default
        inputs.sops-nix.nixosModules.sops
      ];
      
      mkSystem = mkSystemCustomModules commonNixosModules;
    in
    {
      # ===NixOS Configurations===
      nixosConfigurations = {
        default = mkSystem ./hosts/default/configuration.nix;
        "chopin" = mkSystem ./hosts/chopin/configuration.nix;
      };
      
      # ======Development Enviornment===
      devShells = forAllSystems (pkgs: {
        default = import ./devenv.nix { inherit inputs; inherit pkgs; };
      });
    };
}
