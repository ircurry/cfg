{
  config,
  pkgs,
  lib,
  mylib,
  ...
}:

let
  cfg = config.nocturne.graphical.emacs;
  way-cfg = config.nocturne.wayland.editor;
  emacs-overrides = self: super: {
    meow = (
      pkgs.callPackage ./meow.nix {
        inherit (pkgs) fetchFromGitHub;
        inherit (lib) fakeHash;
        inherit (self) trivialBuild;
      }
    );
    popper = (
      pkgs.callPackage ./popper.nix {
        inherit (pkgs) fetchFromGitHub;
        inherit (lib) fakeHash;
        inherit (self) trivialBuild;
      }
    );
  };
  emacs-package =
    with pkgs;
    ((emacsPackagesFor emacs30-pgtk).overrideScope emacs-overrides).emacsWithPackages (
      epkgs:
      let
        packageNames = mylib.filterStrList (str: !mylib.startsWithHash str) (
          mylib.removeEmpty (lib.strings.splitString "\n" (builtins.readFile ./packages.txt))
        );
      in
      (lib.foldl (acc: x: acc ++ [ epkgs."${x}" ]) [ ] packageNames)
      ++ [ epkgs.treesit-grammars.with-all-grammars ]
    );
in
{
  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      programs.emacs = {
        enable = true;
        package = emacs-package;
      };

      services.emacs.enable = false;

      home.sessionVariables.JDTLS_PATH = "${pkgs.jdt-language-server}";
      home.packages = with pkgs; [
        # ===General===
        ispell
        libvterm
        nerd-fonts.jetbrains-mono
        texliveFull
      ];

      home.file = {
        ".emacs.d/early-init.el".source = ./early-init.el;
        ".emacs.d/init.el".source = ./init.el;
        ".emacs.d/cur-config".source = ./cur-config;
        ".emacs.d/cur-lisp".source = ./cur-lisp;
        ".emacs.d/themes".source = ./themes;
        ".emacs.d/dashboard-banners".source = ./dashboard-banners;
      };

      sops.secrets."elfeed_links" = {
        path = "${config.home.homeDirectory}/.emacs.d/cur-elfeed-links";
        format = "binary";
        sopsFile = ../../../secrets/cur-elfeed-links.el;
      };
    })
    (lib.mkIf (way-cfg.name == "emacs") {
      assertions = [
        {
          assertion = cfg.enable == true;
          message = "emacs is set as the default editor on wayland but is not ebabled";
        }
      ];
      nocturne.wayland.startup = [
        {
          name = "emacs";
          exec = "emacs";
          packages = [ config.programs.emacs.package ];
          workspace = 3;
        }
      ];
      nocturne.wayland.editor.exec = "${config.programs.emacs.package}/bin/emacsclient -c -a '${config.programs.emacs.package}/bin/emacs'";
      nocturne.wayland.editor.exec-reuse = "${config.programs.emacs.package}/bin/emacsclient -r";
    })
  ];
}
