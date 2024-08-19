{
  config,
  lib,
  pkgs,
  ...
}:
{
  config = lib.mkMerge [
    (lib.mkIf (config.nocturne.cli.age.enable == true) {
      home.packages = [ pkgs.age ];

    })
    (lib.mkIf (config.nocturne.cli.age.enable == true && config.xdg.enable == true) {
      nocturne.cli.age.keysDir = lib.mkForce 1225 (config.xdg.dataHome + "/age");
    })
  ];
}
