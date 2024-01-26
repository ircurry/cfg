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
        package = pkgs.emacs29-pgtk;
        extraPackages = epkgs: [
          epkgs.autothemer
          epkgs.catppuccin-theme
          epkgs.ccls
          epkgs.company
          epkgs.counsel
          epkgs.denote
          epkgs.doom-themes
          epkgs.eat
          epkgs.flycheck
          epkgs.gemini-mode
          epkgs.helpful
          epkgs.hydra
          epkgs.ivy
          epkgs.ivy-rich
          epkgs.ivy-xref
          epkgs.lsp-java
          epkgs.lsp-mode
          epkgs.lsp-ui
          epkgs.magit
          epkgs.meow
          epkgs.nix-mode
          epkgs.org-bullets
          epkgs.ox-gemini
          epkgs.projectile
          epkgs.rainbow-delimiters
          epkgs.rg
          epkgs.swiper
          epkgs.treemacs
          epkgs.vterm
          epkgs.which-key
          epkgs.yaml-mode
          epkgs.yuck-mode
          epkgs.zoxide
        ];
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
      nocturne.wayland.editor.exec = "${config.programs.emacs.package}/bin/emacsclient -c -a 'emacs'";
      nocturne.wayland.editor.exec-reuse = "${config.programs.emacs.package}/bin/emacsclient -r";
      nocturne.wayland.editor.exec-start = "${config.programs.emacs.package}/bin/emacs --daemon";
    })
  ];
}
