{ ... }: {
  imports = [
    # Options
    ./options

    # Configuration Modules
    ./boot.nix
    ./hyprland.nix
    ./locales.nix
    ./networking.nix
    ./pipewire.nix
    ./plasma.nix
    ./printing.nix
    ./secrets.nix
    ./timezone.nix
    ./user.nix
  ];
}
