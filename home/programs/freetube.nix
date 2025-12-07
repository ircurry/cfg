{
  lib,
  config,
  pkgs,
  inputs,
  ...
}:

let
  cfg = config.nocturne.graphical.freetube;
in
{
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [ freetube ];
  };
}
