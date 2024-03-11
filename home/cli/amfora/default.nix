{ lib, config, pkgs, inputs, ... }:

let cfg = config.nocturne.cli.amfora;
in {
  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.amfora ];

    xdg.configFile."amfora/config.toml" = { source = ./config.toml; };

  };
}
