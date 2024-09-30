{ pkgs, ... }:
{
  config = {
    # Enable CUPS to print documents.
    services.printing = {
      enable = false;
      drivers = with pkgs; [
        gutenprint
        hplip
        splix
      ];
    };
    services.avahi = {
      enable = false;
      nssmdns4 = true;
      publish = {
        enable = true;
        addresses = true;
      };
    };
  };
}
