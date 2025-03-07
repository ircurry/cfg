{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.nocturne.wayland.image;
in
{
  config = lib.mkIf (cfg.name == "imv") {
    programs.imv = {
      enable = true;
      settings = {
        options = {
          overlay_font = "Monospace:12";
        };
      };
    };
    nocturne.wayland.image.exec = "${pkgs.imv}/bin/imv";
    nocturne.wayland.image.exec-dir = "${pkgs.imv}/bin/imv-dir";
  };
}
