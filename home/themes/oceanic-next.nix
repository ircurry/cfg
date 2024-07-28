{ config, lib, ... }:
let
  theme = config.nocturne.themes.theme;
  colors = {
    base00 = "1B2B34"; # ----
    base01 = "343D46"; # ---
    base02 = "4F5B66"; # --
    base03 = "65737E"; # -
    base04 = "A7ADBA"; # +
    base05 = "C0C5CE"; # ++
    base06 = "CDD3DE"; # +++
    base07 = "D8DEE9"; # ~
    base08 = "EC5f67"; # red
    base09 = "F99157"; # orange
    base0A = "FAC863"; # yellow
    base0B = "99C794"; # green
    base0C = "5FB3B3"; # aqua/cyan
    base0D = "6699CC"; # blue
    base0E = "C594C5"; # purple
    base0F = "AB7967"; # brown
  };
in
{
  config = lib.mkIf (theme == "oceanic-next") {
    nocturne = {
      themes.variant = "dark";
      themes.colors = colors;
      wayland.mako = {
        border-color = colors.base0D + "FF";
        progress-color = colors.base0D + "FF";
      };
    };
  };
}
