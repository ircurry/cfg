{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (pkgs) swww writeShellApplication;
  cfg = config.nocturne.wayland.wallpaper;
  startup = writeShellApplication {
    name = "swww-startup-script";
    runtimeInputs = [ swww ];
    text = ''
      setsid -f swww-daemon >/dev/null 2>&1
    '';
  };
  change-wallpaper = writeShellApplication {
    name = "swww-change-wallpaper";
    runtimeInputs = [ swww ];
    text = ''
      swww img -t any "$1"
    '';
  };
in
{
  config = lib.mkIf (cfg.name == "swww") {
    home.packages = [
      swww
    ];
    nocturne.wayland = {
      startup = [
        {
          name = "swww-startup";
          exec = "${lib.getExe startup}";
          packages = [
            swww
            startup
          ];
        }
      ];
      wallpaper.exec-change = "${lib.getExe change-wallpaper}";
    };
  };
}
