{ config, pkgs, lib, ... }:

let
  cfg = config.nocturne.graphical.emacs;
  way-cfg = config.nocturne.wayland.editor; 
in
{
  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      programs.emacs = { 
        enable = true;
        package = cfg.pkg;
      };
      
      home.packages = with pkgs; [
        ccls
        ispell
        jdt-language-server
        libvterm
        nerdfonts
        texliveFull
      ];
      
      home.file = {
        ".emacs.d/init.el".source = ./init.el;
        ".emacs.d/cur-config".source = ./cur-config;
        ".emacs.d/cur-lisp".source = ./cur-lisp;
        ".emacs.d/themes".source = ./themes;
      };
    })
    (lib.mkIf (way-cfg.name == "emacs") {
      assertions = [
        {
          assertion = cfg.enable == true;
          message = "emacs is set as the default editor on wayland but is not ebabled";
        }
      ];
      nocturne.wayland.editor.exec = "${cfg.pkg}/bin/emacsclient -c -a 'emacs'";
      nocturne.wayland.editor.exec-reuse = "${cfg.pkg}/bin/emacsclient -r";
      nocturne.wayland.editor.exec-start = "${cfg.pkg}/bin/emacs --daemon";
    })
  ];
}
