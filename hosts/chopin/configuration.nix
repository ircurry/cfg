# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ lib, config, pkgs, inputs, user, ... }:

{
  # ===Imports===
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    # NixOS Configuration Modules
    ../../nixos
  ];

  # ===Nocturne System Configuration===
  noctsys = {
    programs.hyprland.enable = true;
  };

  # ===Don't Change Please===
  system.stateVersion = "23.11"; # Did you read the comment?
}
