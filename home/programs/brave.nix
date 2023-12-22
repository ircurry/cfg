{ lib, config, pkgs, inputs, ... }:

let
  cfg = config.nocturne.browsers.brave;
in
{
  options.nocturne.browsers.brave = with lib; {
    enable = mkEnableOption "Whether to enable brave browser.";
  };
  
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [ brave ];
  };
}
