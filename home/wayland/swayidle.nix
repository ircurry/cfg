{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.nocturne.wayland.idleManager;
in
{
  config = lib.mkIf (cfg.name == "swayidle") {
    nocturne.wayland.startup = [
      {
        exec = "swayidle -w timeout 300 '${config.nocturne.wayland.lock.exec} -f' timeout 360 'systemctl suspend'";
        packages = [ pkgs.swayidle ];
      }
    ];
  };
}
