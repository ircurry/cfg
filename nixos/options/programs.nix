{ lib, ... }:
{
  options.noctsys.programs = {
    hyprland.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable Hyprland for the system";
    };
  };
}
