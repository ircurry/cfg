{
  pkgs,
  lib,
  host,
  ...
}:
{
  config = lib.mkIf (host == "chopin") {
    hardware.bluetooth = {
      enable = true;
      package = pkgs.bluez;
      powerOnBoot = true;
    };
    services.blueman.enable = true;
  };
}
