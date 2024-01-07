{ config, lib, pkgs, ... }: let
  cfg = config.nocturne.themes.qt;
in {
  config = lib.mkIf cfg.enable {
    qt = {
      enable = true;
      platformTheme = "gtk";
      style.package = pkgs.adwaita-qt;
      style.name = "adwaita-dark";
    };
  };
}
