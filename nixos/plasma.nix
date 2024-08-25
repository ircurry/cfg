{ config, lib, ... }:
let
  cfg = config.hm.nocturne.wayland.compositor;
in
{
  config = lib.mkIf (cfg.name == null) {
    services.xserver = {
      enable = true;

      # Enable the KDE Plasma Desktop Environment.
      displayManager.sddm.enable = true;
      desktopManager.plasma5.enable = true;

      # Configure keymap in X11
      layout = "us";
      xkbVariant = "";
    };
  };
}
