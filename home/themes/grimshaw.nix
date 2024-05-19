{ config, lib, ... }:
let
  theme = config.nocturne.themes.theme;
  bg0 = "1C1F32";
  bg1 = "25283D";
  bg2 = "2B2F45";
  bg3 = "35384E";
  fg0 = "D9E1D3";
  fg1 = "D5D7E0";
  fg2 = "A6A9C4";
  brown = "6C3718";
  burgundy = "531731";
  red = "BF2A31";
  orange = "D58627";
  yellow = "E5C924";
  green = "7FA561";
  aqua = "509E86";
  blue = "4F98A7";
  purple = "A694C4";
  dark-red = "AB2023";
  dark-orange = "CA6924";
  dark-yellow = "BA9D40";
  dark-green = "6E8B58";
  dark-aqua = "46766F";
  dark-blue = "376E83";
  dark-purple = "99768F";
in
{
  config = lib.mkIf (theme == "grimshaw") {
    nocturne = {
      themes.variant = "dark";
      themes.colors = {
        base00 = bg0; # ----
        base01 = bg1; # ---
        base02 = bg2; # --
        base03 = bg3; # -
        base04 = fg0; # +
        base05 = fg1; # ++
        base06 = fg2; # +++
        base07 = burgundy; # ~
        base08 = red; # red
        base09 = orange; # orange
        base0A = yellow; # yellow
        base0B = green; # green
        base0C = aqua; # aqua/cyan
        base0D = blue; # blue
        base0E = purple; # purple
        base0F = brown; # brown
      };
    };
  };
}
