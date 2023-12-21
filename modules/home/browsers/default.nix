{ config, pkgs, inputs, ... }:

{
  imports = [
    ./amfora
    ./brave.nix
    ./firefox.nix
    ./tor-browser.nix
  ];
}
