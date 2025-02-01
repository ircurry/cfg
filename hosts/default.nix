{
  inputs,
  isNixos ? true,
  lib,
  self,
  user ? "recur",
  mylib,
  ...
}:
let
  # ===Extra Helpers===
  pkgsFor = sys: inputs.nixpkgs.legacyPackages.${sys};

  # ===NixOS Builder Functions and Values===
  mkSystem =
    {
      host,
      sys ? "x86_64-linux",
      modules ? [ ],
      isLaptop ? false,
    }@configArgs:
    let
      pkgs = pkgsFor sys;
      pkgsUnfree = (
        import inputs.nixpkgs {
          system = sys;
          config.allowUnfree = true;
        }
      );
      extraSpecialArgs = {
        inherit
          inputs
          isNixos
          self
          user
          mylib
          host
          pkgsUnfree
          isLaptop
          configArgs
          ;
      };
    in
    inputs.nixpkgs.lib.nixosSystem {
      inherit pkgs;
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
        # Alias for home-manager, stolen from iynaix
        # (https://github.com/iynaix/dotfiles)
        (lib.mkAliasOptionModule
          [ "hm" ]
          [
            "home-manager"
            "users"
            user
          ]
        )
        (lib.mkAliasOptionModule
          [ "nocturne" "hm" ]
          [
            "home-manager"
            "users"
            user
            "nocturne"
          ]
        )
        # Extra Modules
      ] ++ modules;
    };
in
{
  # ===NixOS Configurations===
  chopin = mkSystem {
    host = "chopin";
    isLaptop = true;
  };
}
