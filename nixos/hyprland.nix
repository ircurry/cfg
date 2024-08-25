{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.hm.nocturne.wayland.compositor;
in
{
  config = lib.mkIf (cfg.name == "hyprland") {
    services.xserver.desktopManager.gnome.enable = lib.mkForce false;
    services.xserver.displayManager.lightdm.enable = lib.mkForce false;
    programs.hyprland = {
      enable = true;
      portalPackage = pkgs.xdg-desktop-portal-hyprland;
      xwayland.enable = true;
    };
    security.pam.services.swaylock.text = "auth include login";
    xdg.portal = {
      enable = true;
      extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    };
  };
}
