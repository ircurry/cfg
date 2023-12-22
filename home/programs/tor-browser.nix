{ lib, config, pkgs, inputs, ... }:

{
  config = lib.mkIf config.nocturne.graphical.tor-browser.enable {
    home.packages = with pkgs; [ tor-browser ];
  };
}
