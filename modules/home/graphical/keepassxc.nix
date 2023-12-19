{ config, lib, pkgs, ... }:

let
  cfg = config.nocturne.graphical.keepassxc;
in
{
  options.nocturne.graphical.keepassxc = {
    enable = lib.mkEnableOption "Whether to enable keepassxc";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      keepassxc
    ];
  };

}
