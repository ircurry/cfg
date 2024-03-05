{ config, lib, ... }:
let
  theme = config.nocturne.themes.theme;
  nord00 = "2e3440";
  nord01 = "3b4252";
  nord02 = "434c5e";
  nord03 = "4c566a";
  nord04 = "d8dee9";
  nord05 = "e5e9f0";
  nord06 = "eceff4";
  nord07 = "8fbcbb";
  nord08 = "88c0d0";
  nord09 = "81a1c1";
  nord10 = "5e81ac";
  nord11 = "bf616a";
  nord12 = "d08770";
  nord13 = "ebcb8b";
  nord14 = "a3be8c";
  nord15 = "b48ead";
in
{
  config = lib.mkIf (theme == "nord-aurora") {
    nocturne = {
      themes.variant = "dark";
      themes.colors = {
        base00 = nord00; # ----
        base01 = nord01; # ---
        base02 = nord02; # --
        base03 = nord03; # -
        base04 = nord05; # ++
        base05 = nord04; # +
        base06 = nord06; # +++
        base07 = nord07; # ~
        base08 = nord11; # red
        base09 = nord12; # orange
        base0A = nord13; # yellow
        base0B = nord14; # green
        base0C = nord08; # aqua/cyan
        base0D = nord09; # blue
        base0E = nord15; # purple
        base0F = nord10; # brown
      };
      graphical = {
        alacritty.bright-cyan = nord07;
        # alacritty.bright-white = nord06;
        rofi.border-color = nord10;
      };
      wayland.hyprland = {
        col-active-border1 = nord10 + "ee";
      };
      wayland.waybar = {
        launcher-bg = nord10;
        #mpd-fg = nord08;
      };
      wayland.mako = {
        border-color = nord10 + "FF";
        progress-color = nord10 + "FF";
      };
      wayland.swaylock-effects = {
        key-press = nord10;
        ring-ver = nord10;
      };
    };
  };
}
