{ config, lib, ... }:
let
  theme = config.nocturne.themes.theme;
  bg = "282828";
  red = "cc241d";
  green = "98971a";
  yellow = "d79921";
  blue = "458588";
  purple = "b16286";
  aqua = "689d6a";
  orange = "d65d0e";
  light-bg = "928374";
  light-red = "fb4934";
  light-green = "b8bb26";
  light-yellow = "fabd2f";
  light-blue = "83a598";
  light-purple = "d3869b";
  light-aqua = "8ec07c";
  light-orange = "fe8019";
  dark-red = "9d0006";
  dark-yellow = "b57614";
  bg0_h = "1d2021";
  bg0 = "282828";
  bg0_s = "32302f";
  bg1 = "3c3836";
  bg2 = "504945";
  bg3 = "665c54";
  bg4 = "7c6f64";
  grey = "928374";
  fg0 = "fbf1c7";
  fg1 = "ebdbb2";
  fg2 = "d5c4a1";
  fg3 = "bdae93";
  fg4 = "a89984";
in
{
  config = lib.mkIf (theme == "gruvbox-dark") {
    nocturne = {
      themes.variant = "dark";
      themes.colors = {
        base00 = bg0; # ----
        base01 = bg1; # ---
        base02 = bg2; # --
        base03 = bg3; # -
        base04 = fg3; # +
        base05 = fg1; # +++
        base06 = fg2; # ++
        base07 = fg0; # ++++
        base08 = red; # red
        base09 = orange; # orange
        base0A = yellow; # yellow
        base0B = green; # green
        base0C = aqua; # aqua/cyan
        base0D = blue; # blue
        base0E = purple; # purple
        base0F = grey; # brown
      };
      graphical = {
        alacritty = {
          bg = bg;
          #  red = light-red;
          #  yellow = light-yellow;
          #  green = light-green;
          #  cyan = light-aqua;
          #  blue = light-blue;
          #  magenta = light-purple;
          #  bright-black = grey;
        };
        rofi.border-color = bg2;
      };
      wayland.hyprland = {
        col-active-border1 = bg3 + "ee";
        col-active-border2 = bg2 + "ee";
        col-inactive-border = bg1 + "ee";
      };
      wayland.waybar = {
        power-bg = dark-red;
        mpd-fg = aqua;
        launcher-bg = dark-yellow;
        default-bg = bg0_s;
      };
    };
  };
}
