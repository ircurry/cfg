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
                [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && source "$EAT_SHELL_INTEGRATION_DIR/bash"
                if [[ "$INSIDE_EMACS" = 'vterm' ]] \
                  && [[ -n ''${EMACS_VTERM_PATH} ]] \
                  && [[ -f ''${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh ]]; then
        	        source ''${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh
                fi
      '';
      profileExtra = config.nocturne.wayland.compositor.profileExtra;
    };
  };
}
