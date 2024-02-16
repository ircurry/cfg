{ config, lib, ... }:
let
  theme = config.nocturne.themes.theme;
  rosewater = "f5e0dc";
  flamingo = "f2cdcd";
  pink = "f5c2e7";
  mauve = "cba6f7";
  red = "f38ba8";
  maroon = "eba0ac";
  peach = "fab387";
  yellow = "f9e2af";
  green = "a6e3a1";
  teal = "94e2d5";
  sky = "89dceb";
  sapphire = "74c7ec";
  blue = "89b4fa";
  lavender = "b4befe";
  text = "cdd6f4";
  subtext1 = "bac2de";
  subtext0 = "a6adc8";
  overlay2 = "9399b2";
  overlay1 = "7f849c";
  overlay0 = "6c7086";
  surface2 = "585b70";
  surface1 = "45475a";
  surface0 = "313244";
  base = "1e1e2e";
  mantle = "181825";
  crust = "11111b";
in
{
  config = lib.mkIf (theme == "catppuccin-mocha") {
    nocturne = {
      themes.colors = {
        base00 = base; # ----
        base01 = surface0; # ---
        base02 = surface1; # --
        base03 = surface2; # -
        base04 = overlay0; # +++
        base05 = text; # ++++
        base06 = subtext0; # ++
        base07 = overlay2; # ~
        base08 = red; # red
        base09 = peach; # orange
        base0A = yellow; # yellow
        base0B = green; # green
        base0C = teal; # aqua/cyan
        base0D = blue; # blue
        base0E = lavender; # purple
        base0F = mantle; # brown
      };
      wayland.hyprland = {
        col-active-border1 = blue + "ee";
        col-active-border2 = lavender + "ee";
      };
    };
  };
}
