{
  config,
  lib,
  inputs,
  ...
}:
let
  way-cfg = config.nocturne.wayland;
  comp-name = way-cfg.compositor.name;
in
{
  imports = [ inputs.niri.homeModules.niri ];
  config = lib.mkIf (comp-name == "niri") {
    programs.niri.enable = true;
  };
}
