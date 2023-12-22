{ lib, config, pkgs, inputs, ... }:

let
  cfg = config.nocturne.graphical.brave;
in
{
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [ brave ];
  };
}
