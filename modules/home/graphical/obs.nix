{ config, lib, pkgs, ... }:

let
  cfg = config.nocturne.graphical.obs;
in
{
  options.nocturne.graphical.obs = {
    enable = lib.mkEnableOption "Whether to enable obs";
  };

  config = lib.mkIf cfg.enable {
    programs.obs-studio = {
      enable = true;
    };
  };

}
