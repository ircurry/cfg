{
  config,
  pkgs,
  lib,
  ...
}:

{
  config = {
    programs.bash = {
      enable = true;
      initExtra = ''
        PS1="\e[1;31m[\e[1;33m\u\e[1;32m@\e[1;34m\h\e[m \e[1;35m\w\e[1;31m]\e[m\e[1;37m$\e[m "
      '';
      profileExtra = config.nocturne.wayland.compositor.profileExtra;
    };
  };
}
