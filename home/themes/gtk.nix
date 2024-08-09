{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.nocturne.themes.gtk;
  variant = config.nocturne.themes.variant;
in
{
  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      home.pointerCursor = {
        package = pkgs.adwaita-icon-theme;
        name = "Adwaita";
        size = 24;
        gtk.enable = true;
        x11.enable = true;
      };
      gtk = {
        enable = true;

        theme.package = pkgs.gnome-themes-extra;
        theme.name = "Adwaita";

        iconTheme.package = pkgs.adwaita-icon-theme;
        iconTheme.name = "Adwaita";

        font.package = pkgs.dejavu_fonts;
        font.name = "DejaVu Sans";
      };
    })
    (lib.mkIf (cfg.enable && (variant == "dark")) {
      gtk = {
        gtk3.extraConfig = {
          gtk-application-prefer-dark-theme = true;
        };
        gtk4.extraConfig = {
          gtk-application-prefer-dark-theme = true;
        };
      };
    })
  ];
}
