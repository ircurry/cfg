{ config, pkgs, inputs, ... }:

{
  imports = [
    ./brave.nix
    ./firefox.nix
    ./tor-browser.nix
  ];
}
