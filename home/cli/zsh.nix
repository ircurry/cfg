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
    profileExtra = ''
    [[ $(tty) == /dev/tty1 ]] && exec Hyprland && exit 0
    '';
    initExtraFirst = ''
    autoload -U colors && colors	# Load colors
    # zoxide
    eval "$(zoxide init zsh)"
    '';
    initExtra = ''
    clear
    ${pkgs.nitch}/bin/nitch
    '';
    localVariables = {
      PS1 = "%B\%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%m %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}%b$ ";
    };
    shellAliases = {
      cl = "clear";
      info = "info --vi-keys";
      icd = "__zoxide_zi";

      diff = "diff --color=auto";
      grep = "grep --color=auto";

      rm = "rm -i";
      ls = "${pkgs.eza}/bin/eza -h --group-directories-first --icons";
      ll = "${pkgs.eza}/bin/eza -h --group-directories-first --icons -L 1 -T -l";
      la = "${pkgs.eza}/bin/eza -h --group-directories-first --icons -L 1 -T -l -a";
      tree = "${pkgs.eza}/bin/eza -h --group-directories-first --icons -L 2 -T -l";
      mv = "mv -i";
      cp = "cp -i";
      tp = "${pkgs.trashy}/bin/trash put";
      # zc = "zuluCrypt-cli";
      # zm = "zuluMoutn-cli";
      zo = "zoxide";

      ip = "ip -color=auto";
      wget = "${pkgs.wget}/bin/wget --no-hsts";

      am = "${pkgs.amfora}/bin/amfora";
      irssi = "${pkgs.irssi}/bin/irssi --config=$XDG_CONFIG_HOME/irssi/config --home=$XDG_DATA_HOME/irssi";
    };
    
  };
}
