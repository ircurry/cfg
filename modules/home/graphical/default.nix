{ pkgs, ... }:

{
  imports = [
    ./alacritty.nix
    ./keepassxc.nix
    ./flatpak.nix
    ./kid3.nix
    ./obs.nix
    ./signal.nix
  ];

}
