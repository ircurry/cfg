{ config, pkgs, lib, ... }:

let cfg = config.nocturne.cli.shell;
in {
  config = lib.mkIf (cfg.name == "fish") {
    nocturne.cli.shell.exec = "${lib.getExe pkgs.fish}";
    programs.fish = {
      enable = true;
      functions = {
        fish_prompt = ''
          string join ''' -- (set_color -o) (set_color red) '[' (set_color yellow) $USER (set_color green) '@' (set_color blue) $hostname ' ' (set_color magenta) (prompt_pwd) (set_color red) ']' (set_color normal) '$ '
        '';
      };

      shellInit = ''
        set fish_greeting
        ${pkgs.zoxide}/bin/zoxide init fish | source
        direnv hook fish | source
        my-fetch
      '';
    };
  };
}
