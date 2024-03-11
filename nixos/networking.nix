{ host, lib, ... }:
{
  config = lib.mkMerge [
    ({
      networking.hostName = "${host}"; # Define your hostname.
      networking.networkmanager.enable = true;
    })
  ];
}
