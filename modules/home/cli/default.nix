{ config, pkgs, inputs, ... }:

{
  imports = [
    ./core.nix
    ./git.nix
    # ./lf
  ];
}
