{ config, lib, pkgs, ... }:

let
  cfg = config.nocturne.graphical.signal-desktop;
in
{
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      signal-desktop
    ];
  };

}
