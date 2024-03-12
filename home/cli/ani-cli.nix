{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.nocturne.cli.ani-cli;
in
{
  config = lib.mkIf cfg.enable { home.packages = with pkgs; [ ani-cli ]; };
}
