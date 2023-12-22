{ config, lib, pkgs, ... }:

let
  cfg = config.nocturne.graphical.kid3;
in
{
  options.nocturne.graphical.kid3 = {
    enable = lib.mkEnableOption "Whether to enable kid3";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      kid3
    ];
  };

}
