{ config, lib, ... }:
let
  theme = config.nocturne.themes.theme;
  colors = config.nocturne.themes.colors;
  waybar = config.nocturne.wayland.waybar;
  fw-base1 = "605a52";
  fw-base2 = "93836c";
  fw-base3 = "b9a992";
  fw-base4 = "dcd3c6";
  fw-base5 = "e4ddd2";
  fw-base6 = "f1ece4";
  fw-base7 = "f7f3ee";

  fw-accent = "6a4dff";

  fw-orange-text = "5b5143";
  fw-orange-text-sec = "957f5f";
  fw-orange = "f08c00";
  fw-orange-blend = "f7e0c3";

  fw-red-text = "5b4343";
  fw-red-text-sec = "955f5f";
  fw-red = "f00000";
  fw-red-blend = "f6cfcb";

  fw-green-text = "525643";
  fw-green-text-sec = "81895d";
  fw-green = "84bd00";
  fw-green-blend = "e2e9c1";

  fw-teal-text = "465953";
  fw-teal-text-sec = "5f8c7d";
  fw-teal = "00bda4";
  fw-teal-blend = "d2ebe3";

  fw-blue-text = "4c5361";
  fw-blue-text-sec = "7382a0";
  fw-blue = "75a3ff";
  fw-blue-blend = "dde4f2";

  fw-purple-text = "614c61";
  fw-purple-text-sec = "9c739c";
  fw-purple = "ce5cff";
  fw-purple-blend = "f1ddf1";
in
{
  config = lib.mkIf (theme == "flatwhite") {
    nocturne = {
      themes.variant = "light";
      themes.colors = {
        base00 = "f7f3ee"; # ----
        base01 = "f1ece4"; # ---
        base02 = "e4ddd2"; # --
        base03 = "dcd3c6"; # -
        base04 = "b9a992"; # +
        base05 = "605a52"; # ++
        base06 = "93836c"; # +++
        base07 = "999999"; # ~
        base08 = "955f5f"; # red
        base09 = "957f5f"; # orange
        base0A = "957f5f"; # yellow
        base0B = "81895d"; # green
        base0C = "5f8c7d"; # aqua/cyan
        base0D = "7382a0"; # blue
        base0E = "9c739c"; # purple
        base0F = "6a4dff"; # brown
      };
      wayland.dunst = {
        bg = colors.base00;
        bg-opacity = "dd";
      };
      wayland.waybar = {
        launcher-fg = fw-blue-blend;
        launcher-bg = fw-blue-text-sec;
        battery-fg = fw-green-text-sec;
        battery-bg = fw-green-blend;
        battery-warning = fw-orange-text-sec;
        battery-warning-bg = fw-orange-blend;
        battery-critical = fw-red-text-sec;
        battery-critical-bg = fw-red-blend;
        mpd-fg = fw-blue-text-sec;
        mpd-bg = fw-blue-blend;
        backlight-fg = fw-orange-text-sec;
        backlight-bg = fw-orange-blend;
        tray-bg = colors.base02;

        workspace-bg = colors.base00;
        workspace-hover-bg = colors.base02;
        workspace-visible = fw-purple-text-sec;
        workspace-visible-bg = fw-purple-blend;

        memory-fg = fw-purple-text-sec;
        memory-bg = fw-purple-blend;
        cpu-fg = fw-blue-text-sec;
        cpu-bg = fw-blue-blend;
        audio-fg = fw-green-text-sec;
        audio-bg = fw-green-blend;
        network-fg = fw-orange-text-sec;
        network-disconnected = colors.base04;
        network-bg = fw-orange-blend;
        clock-fg = fw-red-text-sec;
        clock-bg = fw-red-blend;
        power-fg = fw-red-blend;
        power-bg = fw-red-text-sec;
      };
      wayland.hyprland = {
        col-active-border1 = colors.base04 + "ee";
        col-active-border2 = colors.base04 + "ee";
        col-inactive-border = colors.base03 + "ee";
      };
    };
  };
}
