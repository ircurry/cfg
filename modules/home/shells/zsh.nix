# note: if zsh is the default shell, disabling
# this module with only disable the configuration
# for zsh, not zsh as the shell.

{ config, lib, pkgs, ... }:

let
  c = config.xdg.configHome;
  d = config.xdg.dataHome;
  cache = config.xdg.cacheHome;
  home = "$HOME";
in
{
  home.sessionVariables = {
    TMUX_TMPDIR = "$XDG_RUNTIME_DIR";
    GNUPGHOME = d + "/gnupg";
    LESSHISTFILE = "/dev/null";
    GOPATH = home + "/.local/bin/go/";
    EDITOR = "vim";
    TERMINAL = "alacritty";
    FZF_DEFAULT_OPTS = "--color='prompt:3,pointer:3,bg+:0,fg+:6,hl:2,hl+:3:bold,header:3' --reverse --border --prompt='# ' --bind=alt-1:first,alt-2:last";
  };

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    syntaxHighlighting.enable = true;
    defaultKeymap = "emacs";
    initExtraFirst = ''
    autoload -U colors && colors	# Load colors
    '';
    localVariables = {
      PS1 = "%B\%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%m %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}%b$ ";
    };
  };
}
