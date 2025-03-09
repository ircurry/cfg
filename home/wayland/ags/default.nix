{
  config,
  inputs,
  lib,
  ...
}:
let
  wcfg = config.nocturne.wayland;
  inherit (wcfg) bar;
in
{
  config = lib.mkMerge [
    (lib.mkIf (bar.name == "ags") { })
  ];
}
