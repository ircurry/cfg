{ config, lib, ... }:
let
  inherit (config) xdg;
  inherit (config.nocturne.wayland) monitor-profiles;
in
{
  config = lib.mkMerge [
    (lib.mkIf (monitor-profiles != [ ] && xdg.enable == true) {
      xdg.configFile."nocturne/monitors.json".text = builtins.toJSON monitor-profiles;
    })
    (lib.mkIf (monitor-profiles != [ ] && xdg.enable == false) {
      home.file.".config/nocturne/monitors.json".text = builtins.toJSON monitor-profiles;
    })
  ];
}
