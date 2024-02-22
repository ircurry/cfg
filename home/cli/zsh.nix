{ config, lib, pkgs, ... }:

let
  cfg = config.nocturne.cli.shell;
in
{
  config = lib.mkIf (cfg.name == "zsh") {
    nocturne.cli.shell.exec = "${lib.getExe pkgs.zsh}";
    programs.zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;
      syntaxHighlighting.enable = true;
      defaultKeymap = "emacs";
      profileExtra = ''
      [[ $(tty) == /dev/tty1 ]] && exec Hyprland && exit 0
      '';
      initExtraFirst = ''
      autoload -U colors && colors	# Load colors
             # zoxide
      eval "$(zoxide init zsh)"
      '';
      initExtra = ''
      ${pkgs.nitch}/bin/nitch
      '';
      localVariables = {
        PS1 = "%B\%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%m %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}%b$ ";
      };
    };
  };
}
