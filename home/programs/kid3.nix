{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.nocturne.graphical.kid3;
in
{
  config = lib.mkIf cfg.enable { home.packages = with pkgs; [ kid3 ]; };
}
