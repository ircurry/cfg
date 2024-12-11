{ pkgs, lib, ... }:
{
  config = lib.mkIf false {
    # Enable CUPS to print documents.
    services.printing = {
      enable = true;
      drivers = with pkgs; [
        gutenprint
        hplip
        splix
      ];
    };
    services.avahi = {
      enable = true;
      nssmdns4 = true;
      publish = {
        enable = true;
        addresses = true;
      };
    };
  };
}
