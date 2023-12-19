{ config, lib, pkgs, ... }:

let
  cfg = config.nocturne.graphical.signal-desktop;
in
{
  options.nocturne.graphical.signal-desktop = {
    enable = lib.mkEnableOption "Whether to enable signal-desktop";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      signal-desktop
    ];
  };

}
