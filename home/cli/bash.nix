{ config, pkgs, lib, ... }:

{
  config = {
    programs.bash = {
      enable = true;
      profileExtra = config.nocturne.wayland.compositor.profileExtra;
    };
  };
}
