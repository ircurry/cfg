{ lib, config, pkgs, inputs, ... }:

{
  config = lib.mkIf config.nocturne.graphical.torBrowser.enable {
    home.packages = with pkgs; [ tor-browser ];
  };
}
