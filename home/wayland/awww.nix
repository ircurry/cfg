{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (pkgs) awww writeShellApplication;
  cfg = config.nocturne.wayland.wallpaper;
  startup = writeShellApplication {
    name = "awww-startup-script";
    runtimeInputs = [ awww ];
    text = ''
      setsid -f awww-daemon >/dev/null 2>&1
    '';
  };
  change-wallpaper = writeShellApplication {
    name = "awww-change-wallpaper";
    runtimeInputs = [ awww ];
    text = ''
      awww img -t any "$1"
    '';
  };
in
{
  config = lib.mkIf (cfg.name == "awww") {
    home.packages = [
      awww
    ];
    nocturne.wayland = {
      startup = [
        {
          name = "awww-startup";
          exec = "${lib.getExe startup}";
          packages = [
            awww
            startup
          ];
        }
      ];
      wallpaper.exec-change = "${lib.getExe change-wallpaper}";
    };
  };
}
