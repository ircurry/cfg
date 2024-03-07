{ config, pkgs, lib, ... }:

let
  cfg = config.nocturne.cli.scripts.youtubeScripts;
in lib.mkIf cfg.enable {
  home.packages = [ pkgs.custom.youtubeScripts ];
}
