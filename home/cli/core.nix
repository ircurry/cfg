{ pkgs, ... }: {
  home.packages = with pkgs; [
    bottom   # system resource monitor
    eza      # a smart ls replacement
    file     # file info
    fzf      # cli fuzzy finder
    neofetch # system info fetcher
    nitch    # fancier system info fetcher
    trashy   # fast trash-cli replacement
    vim      # cli text editor (I don't want to use nano)
    zoxide   # a smart cd replacement
  ];
}
