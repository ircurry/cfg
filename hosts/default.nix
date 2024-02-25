{
  inputs,
  isNixos ? true,
  lib,
  self,
  user ? "recur",
  ...
}:
let
  # ===Extra Helpers===
  pkgsFor = sys: inputs.nixpkgs.legacyPackages.${sys};

  # ===NixOS Builder Functions and Values===
  mkSystemCustomModules = modules: host: sys: let
    pkgs = pkgsFor sys;
    extraSpecialArgs = {
      inherit inputs self isNixos user host;
      isLaptop = host == "chopin";
    };
  in inputs.nixpkgs.lib.nixosSystem {
    specialArgs = extraSpecialArgs;
    modules = [
      # Host Configuration
      ./${host}/configuration.nix
      # Home-Manager Modules Configuration
      inputs.home-manager.nixosModules.default
      { home-manager = { inherit extraSpecialArgs; }; }
      # Extra Modules
    ] ++ modules;
  };

  mkSystem = mkSystemCustomModules [];
  
in {
  # ===NixOS Configurations===
  chopin = mkSystem "chopin" "x86_64";
  default = mkSystem "default" "x86_64";
}
