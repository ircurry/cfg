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
  };
}
