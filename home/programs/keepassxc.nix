{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.nocturne.graphical.keepassxc;
in
{
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [ keepassxc ];
    nocturne.wayland.startup = [
      {
        name = "keepassxc";
        exec = "keepassxc";
        packages = [ pkgs.keepassxc ];
        workspace = 5;
      }
    ];
  };
}
