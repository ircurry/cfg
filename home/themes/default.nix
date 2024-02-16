{ ... }: {
  imports = [
    # ===Took Kits===
    ./gtk.nix
    ./qt.nix
    
    # ===Themes===
    ./basic-dark.nix
    ./catppuccin-mocha.nix
    ./gruvbox.nix
    ./kanagawa-wave.nix
    ./nord-aurora.nix
  ];
}
