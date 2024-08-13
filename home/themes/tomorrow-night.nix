{ config, lib, ... }:
let
  theme = config.nocturne.themes.theme;
in
{
  config = lib.mkIf (theme == "tomorrow-night") {
    nocturne = {
      themes.variant = "dark";
      themes.colors = {
        base00 = "1d1f21"; # ----
        base01 = "282a2e"; # ---
        base02 = "373b41"; # --
        base03 = "969896"; # -
        base04 = "b4b7b4"; # +
        base05 = "c5c8c6"; # ++
        base06 = "e0e0e0"; # +++
        base07 = "ffffff"; # ~
        base08 = "cc6666"; # red
        base09 = "de935f"; # orange
        base0A = "f0c674"; # yellow
        base0B = "b5bd68"; # green
        base0C = "8abeb7"; # aqua/cyan
        base0D = "81a2be"; # blue
        base0E = "b294bb"; # purple
        base0F = "663300"; # brown
      };
      wayland.waybar = {
        workspace-empty = config.nocturne.themes.colors.base02;
      };
    };
  };
}
