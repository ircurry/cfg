{ config, lib, ... }:
let
  theme = config.nocturne.themes.theme;
  fujiWhite = "DCD7BA";
  oldWhite = "c8c093";
  sumiInk0 = "16161d";
  sumiInk1 = "1f1f28";
  sumiInk2 = "2a2a37";
  sumiInk3 = "363646";
  sumiInk4 = "54546d";
  waveBlue1 = "223249";
  waveBlue2 = "2d4f67";
  winterGreen = "2b3328";
  winterYellow = "49443c";
  winterRed = "43242b";
  winterBlue = "252535";
  autumnGreen = "76946a";
  autumnRed = "c34043";
  autumnYellow = "dca561";
  samuriRed = "e82424";
  roninYellow = "ff9e3b";
  waveAqua1 = "6a9589";
  dragonBlue = "658594";
  fujiGray = "727169";
  springViolet1 = "938aa9";
  oniViolet = "957fb8";
  cyrstalBlue = "7e9cd8";
  springViolet2 = "9cabca";
  springBlue = "7fb4ca";
  lightBlue = "a3d4d5";
  waveAqua2 = "7aa89f";
  springGreen = "98bb6c";
  boatYellow1 = "938056";
  boatYellow2 = "c0a36e";
  carpYellow = "e6c384";
  sakuraPink = "d27e99";
  waveRed = "e46876";
  peachRed = "ff5d62";
  surimiOrange = "ffa066";
  katanaGray = "717c7c";
in
{
  config = lib.mkIf (theme == "kanagawa-wave") {
    nocturne = {
      themes.colors = {
        base00 = sumiInk1; # ----
        base01 = sumiInk2; # ---
        base02 = sumiInk3; # --
        base03 = sumiInk4; # -
        base04 = oldWhite; # +
        base05 = fujiWhite; # ++
        base06 = lightBlue; # +++
        base07 = carpYellow; # ~
        base08 = peachRed; # red
        base09 = surimiOrange; # orange
        base0A = carpYellow; # yellow
        base0B = springGreen; # green
        base0C = waveAqua2; # aqua/cyan
        base0D = cyrstalBlue; # blue
        base0E = oniViolet; # purple
        base0F = katanaGray; # brown
      };
      graphical.alacritty = {
        white = oldWhite;
        red = autumnRed;
        yellow = boatYellow2;
        green = autumnGreen;
        cyan = waveAqua1;
        bright-blue = springBlue;
        bright-magenta = springViolet1;
      };
      graphical.rofi = {
        border-color = oniViolet;
      };
      wayland.hyprland = {
        col-active-border1 = oniViolet + "ee";
        col-active-border2 = oniViolet + "ee";
      };
      wayland.mako = {
        progress-color = waveBlue2 + "FF";
      };
      wayland.waybar = {
        workspace-visible = autumnYellow;
      };
    };
  };
}
