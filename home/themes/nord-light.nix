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
  # Alternate values for better contrast
  nord07 = "2C7088";
  nord08 = "398EAC";
  nord09 = "3B6EA8";
  nord10 = "5272AF";
  nord11 = "99324B";
  nord12 = "AC4426";
  nord13 = "9A7500";
  nord14 = "4F894C";
  nord15 = "842879";
in {
  config = lib.mkIf (theme == "nord-light") {
    nocturne = {
      themes.variant = "light";
      themes.colors = {
        base00 = nord06; # +++
        base01 = nord05; # ++
        base02 = nord04; # +
        base03 = nord03; # -
        base04 = nord02; # --
        base05 = nord01; # ---
        base06 = nord00; # ----
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
        alacritty.bg = nord06;
        alacritty.bright-cyan = nord07;
        rofi.bg-selection = nord04;
        rofi.border-color = nord10;
      };
      wayland.hyprland = {
        col-active-border1 = nord10 + "ee";
        col-active-border2 = nord10 + "ee";
        col-inactive-border = nord06 + "ee";
      };
      wayland.waybar = {
        # Right Side
        launcher-bg = nord10;
        launcher-fg = nord06;
        battery-bg = nord06;
        backlight-bg = nord06;
        mpd-bg = nord06;
        tray-bg = nord06;

        # Center
        workspace-bg = nord06;
        workspace-hover-bg = nord04;
        workspace-fg = nord03;
        workspace-empty = nord04;

        # Left
        cpu-bg = nord06;
        memory-bg = nord06;
        audio-bg = nord06;
        network-bg = nord06;
        clock-bg = nord06;
        power-fg = nord06;
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
