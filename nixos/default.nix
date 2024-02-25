{ ... }: {
  imports = [
    # Options
    ./options

    # Configuration Modules
    ./hyprland.nix
    ./locales.nix
    ./plasma.nix
    ./secrets.nix
  ];
}
