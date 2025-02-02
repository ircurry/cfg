{
  lib,
  config,
  pkgs,
  ...
}:

let
  cfg = config.nocturne.graphical.qbittorrent;
in
{
  config = lib.mkIf cfg.enable { home.packages = with pkgs; [ qbittorrent ]; };
}
