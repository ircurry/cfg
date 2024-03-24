{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:

let
  cfg = config.nocturne.graphical.emacs;
  way-cfg = config.nocturne.wayland.editor;
  emacs-package =
    with pkgs;
    ((emacsPackagesFor emacs-pgtk).emacsWithPackages (epkgs: [
      ##########################
      ## Tree-Sitter Grammars ##
      ##########################
      epkgs.treesit-grammars.with-all-grammars

      ########################################
      ## Configuration Modules Dependencies ##
      ########################################

      # ===Bindings===
      epkgs.hydra
      epkgs.meow

      # ===C===
      epkgs.ccls

      # ===Completion===
      epkgs.counsel
      epkgs.swiper
      epkgs.ivy
      epkgs.ivy-rich

      # ===Denote===
      epkgs.denote

      # ===Dired===

      # ===Elcord===
      epkgs.elcord

      # ===Elisp===
      epkgs.rainbow-delimiters

      # ===Essentials===

      # ===Faces===
      epkgs.doom-themes
      epkgs.autothemer
      epkgs.catppuccin-theme

      # ===Haskell===
      epkgs.haskell-mode
      epkgs.lsp-haskell
      epkgs.company-ghci

      # ===Help===
      epkgs.helpful
      epkgs.which-key

      # ===IDE===
      epkgs.lsp-mode
      epkgs.lsp-ui
      epkgs.company
      epkgs.flycheck
      epkgs.treemacs
      epkgs.ivy-xref
      epkgs.magit
      epkgs.projectile
      epkgs.rg
      epkgs.envrc
      epkgs.zoxide

      # ===Java===
      epkgs.lsp-java

      # ===Markup===
      epkgs.yaml-mode
      epkgs.yuck-mode

      # ===Nix===
      epkgs.nix-mode

      # ===Org===
      epkgs.org-bullets

      # ===Rust===
      epkgs.rustic

      # ===Shell===
      epkgs.vterm

      # ===Smol-net===
      epkgs.gemini-mode
      epkgs.ox-gemini

      # ===Windows===

      ###############################
      ## Lisp Modules Dependencies ##
      ###############################
    ]));
in
{
  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      programs.emacs = {
        enable = true;
        package = emacs-package;
      };

      services.emacs.enable = true;

      home.sessionVariables.JDTLS_PATH = "${pkgs.jdt-language-server}";
      home.packages = with pkgs; [
        # ===General===
        ispell
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
