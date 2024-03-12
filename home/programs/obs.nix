{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.nocturne.graphical.obs;
in
{
  config = lib.mkIf cfg.enable {
    programs.obs-studio = {
      enable = true;
    };
  };
}
