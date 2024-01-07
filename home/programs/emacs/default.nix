{ config, pkgs, lib, inputs, ... }:

let
  cfg = config.nocturne.editors.emacs;
in
{
  config = lib.mkIf cfg.enable {
    programs.emacs = { 
      enable = true;
      package = cfg.pkg;
    };

    home.packages = with pkgs; [
      ispell
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
