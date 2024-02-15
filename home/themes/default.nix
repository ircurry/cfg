{ ... }: {
  imports = [
    # ===Took Kits===
    ./gtk.nix
    ./qt.nix
    
    # ===Themes===
    ./basic-dark.nix
    ./gruvbox.nix
    ./nord-aurora.nix
  ];
}
