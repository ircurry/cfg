{ ... }: {
  imports = [
    # ===Took Kits===
    ./gtk.nix
    ./qt.nix
    
    # ===Themes===
    ./basic-dark.nix
    ./catppuccin-mocha.nix
    ./gruvbox.nix
    ./nord-aurora.nix
  ];
}
