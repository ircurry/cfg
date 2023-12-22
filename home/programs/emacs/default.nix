{ config, pkgs, lib, inputs, ... }:

let
  cfg = config.nocturne.editors.emacs;
in
{
  options.nocturne.editors.emacs = {
    enable = lib.mkEnableOption "Whether to use emacs as an editor";
  };

  config = lib.mkIf cfg.enable {
    programs.emacs = { 
      enable = true;
      package = pkgs.emacs29-pgtk;
    };

    home.packages = with pkgs; [
      nerdfonts
      texliveFull
    ];
    
    home.file = {
      ".emacs.d/init.el".source = ./init.el;
      ".emacs.d/cur-config".source = ./cur-config;
      ".emacs.d/cur-lisp".source = ./cur-lisp;
      ".emacs.d/themes".source = ./themes;
    };

  };
}
