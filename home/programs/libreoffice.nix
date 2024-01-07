{ config, lib, pkgs, ... }:

let
  cfg = config.nocturne.graphical.libreoffice;
in
{
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      libreoffice-qt
    ];
  };
}
