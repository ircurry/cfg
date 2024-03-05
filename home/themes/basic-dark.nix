{ config, lib, ... }:
let
  theme = config.nocturne.themes.theme;
in
{
  config = lib.mkIf (theme == "basic-dark") {
    nocturne = {
      themes.variant = "dark";
      themes.colors = {
        base00 = "000000"; # ----
        base01 = "222222"; # ---
        base02 = "444444"; # --
        base03 = "666666"; # -
        base04 = "ffffff"; # +
        base05 = "dddddd"; # ++
        base06 = "bbbbbb"; # +++
        base07 = "999999"; # ~
        base08 = "ff0000"; # red
        base09 = "ff9933"; # orange
        base0A = "ffcc00"; # yellow
        base0B = "00cc00"; # green
        base0C = "00ff99"; # aqua/cyan
        base0D = "0000ff"; # blue
        base0E = "cc00cc"; # purple
        base0F = "663300"; # brown
      };
    };
  };
}
