{
  lib,
  config,
  pkgs,
  inputs,
  ...
}:

let
  cfg = config.nocturne.graphical.mullvadBrowser;
in
{
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [ mullvad-browser ];
    nocturne.wayland.startup = [
      {
        exec = "mullvad-browser";
        packages = [ pkgs.mullvad-browser ];
        workspace = 4;
      }
    ];
  };
}
