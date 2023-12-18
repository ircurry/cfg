{ config, pkgs, lib, inputs, ... }:

let
  cfg = config.editors.emacs;
in
{
  options.editors.emacs = {
    enable = lib.mkEnableOption "Whether to use emacs as the editor";
  };

  config = lib.mkIf cfg.enable {
    programs.emacs = { 
      enable = true;
      package = pkgs.emacs29-pgtk;
    };

    home.packages = with pkgs; [
      nerdfonts
    ];
    
    home.file = {
      ".emacs.d/init.el".source = ./init.el;
      ".emacs.d/cur-config".source = ./cur-config;
      ".emacs.d/cur-lisp".source = ./cur-lisp;
      ".emacs.d/themes".source = ./themes;
    };
    

  };
}
