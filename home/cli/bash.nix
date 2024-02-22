{ config, pkgs, lib, ... }:

{
  config = {
    programs.bash = {
      enable = true;
      profileExtra =
        ''
            [[ $(tty) == /dev/tty1 ]] && exec Hyprland && exit 0
        '';
    };
  };
}
