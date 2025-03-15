{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.nocturne.wayland.fileManager;
in
{
  config = lib.mkIf (cfg.name == "thunar") {
    home.packages = [ pkgs.xfce.thunar ];
  };
}
