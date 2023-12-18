{ config, pkgs, lib, inputs, ... }:

let
  cfg = config.nocturne.editors.emacs;
in
{
  options.nocturne.editors.emacs = {
    enable = lib.mkEnableOption "Whether to use emacs as an editor";
    main = lib.mkEnableOption "Whether to set emacs as the main editor";
    server = lib.mkEnableOption "Whether to use emacs daemon";
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

    home.sessionVariables = lib.mkIf cfg.main (if (cfg.server == true ) then {
      EDITOR = "emacsclient -c -a 'emacs'";
    } else {
      EDITOR = "emacs";
    });

    services.emacs = lib.mkIf cfg.server {
      enable = true;
      package = pkgs.emacs29-pgtk;
      client = {
        enable = true;
        arguments = ["-c" "-a" "'emacs'"];
      };
    };
    
  };
}
