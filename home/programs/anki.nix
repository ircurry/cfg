{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.nocturne.graphical.anki;
in
{
  config = lib.mkIf cfg.enable { home.packages = with pkgs; [ anki-bin ]; };
}
