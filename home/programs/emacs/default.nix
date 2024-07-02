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
    ((emacsPackagesFor emacs29-pgtk).emacsWithPackages (
      epkgs: with epkgs; [
        ##########################
        ## Tree-Sitter Grammars ##
        ##########################
        treesit-grammars.with-all-grammars

        ########################################
        ## Configuration Modules Dependencies ##
        ########################################

        # ===Bindings===
        hydra
        meow

        # ===C===
        ccls

        # ===Completion===
        counsel
        swiper
        ivy
        ivy-rich
        vertico
        marginalia
        consult
        orderless
        vertico-posframe
        embark

        # ===Dired===
        nerd-icons-dired

        # ===Elcord===
        elcord

        # ===Elisp===
        rainbow-delimiters
        geiser
        geiser-chez
        geiser-chibi
        geiser-chicken
        geiser-gambit
        geiser-gauche
        geiser-guile
        geiser-kawa
        geiser-mit
        geiser-racket
        geiser-stklos
        macrostep
        macrostep-geiser
        paredit

        # ===Essentials===

        # ===Faces===
        doom-themes
        autothemer
        catppuccin-theme
        ef-themes
        nerd-icons-ibuffer
        spacious-padding

        # ===Haskell===
        haskell-mode
        lsp-haskell
        company-ghci

        # ===Help===
        helpful
        which-key

        # ===IDE===
        lsp-mode
        lsp-ui
        company
        flycheck
        treemacs
        ivy-xref
        magit
        projectile
        rg
        envrc
        just-mode
        justl

        # ===Java===
        lsp-java

        # ===Markup===
        yaml-mode
        yuck-mode

        # ===Nix===
        nix-mode
        nix-ts-mode

        # ===Org===
        org-bullets

        # ===Rust===
        rustic

        # ===Shell===
        vterm
        eat
        zoxide

        # ===Smol-net===
        gemini-mode
        ox-gemini
        elpher

        # ===Windows===

        # ===Zig===
        zig-mode

        ###############################
        ## Lisp Modules Dependencies ##
        ###############################
      ]
    ));
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
