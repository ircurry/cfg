# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ lib, config, pkgs, inputs, user, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ../../nixos
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  noctsys = {
    programs.hyprland.enable = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users."${user}" = {
    isNormalUser = true;
    description = "Ian Curran";
    extraGroups = [ "networkmanager" "wheel" ];
    initialPassword = "password";
    hashedPasswordFile = config.sops.secrets."${user}_password".path;
  };

  home-manager = {
    users = { 
      "${user}" = import ./home.nix; 
    };
  };

  # ===Don't Change Please===
  system.stateVersion = "23.11"; # Did you read the comment?

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
}
