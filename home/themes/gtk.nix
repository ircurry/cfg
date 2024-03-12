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
      gtk = {
        enable = true;

        # cursorTheme.package = pkgs.bibata-cursors;
        # cursorTheme.name = "Bibata-Modern-Ice";

        theme.package = pkgs.gnome.gnome-themes-extra;
        theme.name = "Adwaita";

        iconTheme.package = pkgs.gnome.adwaita-icon-theme;
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
