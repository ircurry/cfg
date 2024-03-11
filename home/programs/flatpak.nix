{ config, lib, pkgs, ... }:

let
  cfg = config.nocturne.graphical.flatpak;
in
{
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      flatpak
    ];

    #TODO: fix adding the environment variables
    xdg.systemDirs.data = [
      "/var/lib/flatpak/exports/share"
      "${config.home.homeDirectory}/.local/share/flatpak/exports/share"
    ];
  };

}
