# note: if zsh is the default shell, disabling
# this module with only disable the configuration
# for zsh, not zsh as the shell.

{ config, lib, pkgs, ... }:

let
  cfg = config.nocturne.shells.zsh;
in
{
  options.nocturne.graphical.zsh = {
    enable = lib.mkEnableOption "Whether to enable zsh as the user shell";
  };

  config = lib.mkIf cfg.enable {
    programs.zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;
      syntaxHighlighting.enable = true;
    };
  };

}
