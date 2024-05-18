{
  config,
  lib,
  pkgsUnfree,
  ...
}:

let
  cfg = config.nocturne.graphical.keymapp;
in
{
  config = lib.mkIf cfg.enable { home.packages = [ pkgsUnfree.keymapp ]; };
}
