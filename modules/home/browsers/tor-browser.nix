{ lib, config, pkgs, inputs, ... }:

{
  options.nocturne.browsers.tor-browser = {
    enable = lib.mkEnableOption "Whether to enable the Tor Browser";
  };
  
  config = lib.mkIf config.nocturne.browsers.tor-browser.enable {
    home.packages = with pkgs; [ tor-browser ];
  };
}
