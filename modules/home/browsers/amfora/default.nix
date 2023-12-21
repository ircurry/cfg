{ lib, config, pkgs, inputs, ... }:

let
  cfg = config.nocturne.browsers.amfora;
in
{
  options.nocturne.browsers.amfora = with lib; {
    enable = mkEnableOption "Whether to enable amfora gemini browser";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.amfora ];

    xdg.configFile."amfora/config.toml" = {
      source = ./config.toml;
      # target = "amfora/config.toml";
    };
    
  };
}
