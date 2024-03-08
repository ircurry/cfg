{ pkgs, ... }:
{
  config = {
    # Enable CUPS to print documents.
    services.printing = {
      enable = true;
      drivers = with pkgs; [ gutenprint hplip splix ];
    };
  };
}
