{ inputs
, isNixos ? true
, lib
, self
, user ? "recur"
, mylib
, ...
}:
let
  # ===Extra Helpers===
  pkgsFor = sys: inputs.nixpkgs.legacyPackages.${sys};

  # ===NixOS Builder Functions and Values===
  mkSystemCustomModules = modules: host: sys: let
    pkgs = pkgsFor sys;
    extraSpecialArgs = {
      inherit inputs isNixos self user mylib host;
      isLaptop = host == "chopin";
    };
  in inputs.nixpkgs.lib.nixosSystem {
    specialArgs = extraSpecialArgs;
    modules = [
      # Host Configuration
      ./${host}/configuration.nix
      # Home-Manager Module Configuration
      inputs.home-manager.nixosModules.default
      # NixOS Configuration Modules
      ../nixos
      # Nixpkgs Overlays
      ../overlays
      {
        home-manager = {
          useGlobalPkgs = true;
          inherit extraSpecialArgs;
        };
      }
      # Alias for home-manager, stolen from iynaix (https://github.com/iynaix/dotfiles)
      (lib.mkAliasOptionModule [ "hm" ] [
        "home-manager"
        "users"
        user
      ])
      # Extra Modules
    ] ++ modules;
  };

  mkSystem = mkSystemCustomModules [];
  
in {
  # ===NixOS Configurations===
  chopin = mkSystem "chopin" "x86_64";
  default = mkSystem "default" "x86_64";
}
