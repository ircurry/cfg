{ config, lib, ... }:
let
  theme = config.nocturne.themes.theme;
  bg = "fbf1c7";
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
  dark-bg = "928374";
  dark-red = "9d0006";
  dark-green = "79740e";
  dark-yellow = "b57614";
  dark-blue = "076678";
  dark-purple = "8f3f71";
  dark-aqua = "427b58";
  dark-orange = "af3a03";
  bg0s = "f2e5bc";
  bg0 = "fbf1c7";
  bg1 = "ebdbb2";
  bg2 = "d5c4a1";
  bg3 = "bdae93";
  bg4 = "a89984";
  grey = "928374";
  fg0 = "282828";
  fg1 = "3c3836";
  fg2 = "504945";
  fg3 = "665c54";
  fg4 = "7c6f64";
in
{
  config = lib.mkIf (theme == "gruvbox-light") {
    nocturne = {
      themes.variant = "light";
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
        rofi.border-color = bg4;
      };
      wayland.hyprland = {
        col-active-border1 = bg4 + "ee";
        col-active-border2 = bg3 + "ee";
        col-inactive-border = bg1 + "ee";
      };
      wayland.waybar = {
        # Left
        launcher-fg = bg0;
        launcher-bg = yellow;
        battery-bg = bg0;
        backlight-bg = bg0;
        mpd-bg = bg0;
        tray-bg = bg0;

        # Center
        workspace-visible = yellow;
        workspace-bg = bg0;
        workspace-hover-bg = bg1;

        # Right
        cpu-bg = bg0;
        memory-bg = bg0;
        audio-bg = bg0;
        network-bg = bg0;
        clock-bg = bg0;
        power-fg = bg0;
      };
    };
  };
}
