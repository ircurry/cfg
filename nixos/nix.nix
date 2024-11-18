{ pkgs, inputs, ... }:
{
  config = {
    documentation = {
      dev.enable = true;
      info.enable = true;
    };
    nix = {
      package = pkgs.nixVersions.latest;
      settings = {
        substituters = [ "https://hyprland.cachix.org" ];
        trusted-public-keys = [ "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc=" ];
        experimental-features = [
          "nix-command"
          "flakes"
          "pipe-operators"
        ];
      };
      nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
    };
  };
}
