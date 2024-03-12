{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.nocturne.graphical.imv;
  img-cfg = config.nocturne.wayland.image;
in
{
  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      programs.imv = {
        enable = true;
        settings = {
          options = {
            overlay_font = "Monospace:12";
          };
        };
      };
    })
    (lib.mkIf (img-cfg.name == "imv") {
      assertions = [
        {
          assertion = cfg.enable == true;
          message = "imv is set as the default image viewer on wayland but is not enabled";
        }
      ];
      nocturne.wayland.image.exec = "${pkgs.imv}/bin/imv";
      nocturne.wayland.image.exec-dir = "${pkgs.imv}/bin/imv-dir";
    })
  ];
}
