{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.nocturne.graphical.keepassxc;
in
{
  config = lib.mkIf cfg.enable { home.packages = with pkgs; [ keepassxc ]; };
}
