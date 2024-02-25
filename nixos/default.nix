{ ... }: {
  imports = [
    # Options
    ./options

    # Configuration Modules
    ./hyprland.nix
    ./plasma.nix
    ./secrets.nix
  ];
}
