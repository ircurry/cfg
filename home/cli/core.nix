{ pkgs, ... }:
{
  # ===Core CLI Packages===
  home.packages = with pkgs; [
    bottom # system resource monitor
    eza # a smart ls replacement
    file # file info
    fzf # cli fuzzy finder
    neofetch # system info fetcher
    nitch # fancier system info fetcher
    ripgrep # fast grep replacement
    tmux # a terminal multiplexer
    trashy # fast trash-cli replacement
    tree # recursive directory tree program
    vim # cli text editor (I don't want to use nano)
    zoxide # a smart cd replacement
  ];
}
