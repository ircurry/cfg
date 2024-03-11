{ config, lib, pkgs, ... }:

let
  cfg = config.nocturne.graphical.signalDesktop;
in
{
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      signal-desktop
    ];
  };

}
