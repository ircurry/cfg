{ config, lib, ... }: let
  cfg = config.nocturne-sys.hyprland;
in {
  config = lib.mkIf cfg.enable {
    services.xserver.desktopManager.gnome.enable = lib.mkForce false;
    services.xserver.displayManager.lightdm.enable = lib.mkForce false;
    programs.hyprland = {
      enable = true;
      xwayland.enable = true;
    };
  };
}