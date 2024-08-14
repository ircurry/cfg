{
  config,
  lib,
  pkgsUnfree,
  ...
}:

let
  cfg = config.nocturne.graphical.veracrypt;
in
{
  config = lib.mkIf cfg.enable { home.packages = [ pkgsUnfree.veracrypt ]; };
}
