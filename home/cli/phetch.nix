{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.nocturne.cli.phetch;
in
{
  config = lib.mkIf cfg.enable { home.packages = with pkgs; [ phetch ]; };
}
