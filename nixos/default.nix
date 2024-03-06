{ ... }: {
  imports = [
    # Options
    ./options

    # Configuration Modules
    ./boot.nix
    ./hyprland.nix
    ./locales.nix
    ./networking.nix
    ./nix.nix
    ./overlays.nix
    ./pipewire.nix
    ./plasma.nix
    ./printing.nix
    ./secrets.nix
    ./timezone.nix
    ./unfree.nix
    ./user.nix
  ];
}
