{
  lib,
  config,
  pkgs,
  inputs,
  user,
  ...
}:

{
  # ===Imports===
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  # ===Nocturne System Configuration===
  nocturne.network.mullvad-vpn.enable = true;

  # ===Don't Change Please===
  system.stateVersion = "23.11"; # Did you read the comment?
}
