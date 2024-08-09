{
  config,
  pkgs,
  lib,
  inputs,
  mylib,
  ...
}:

let
  cfg = config.nocturne.graphical.emacs;
  way-cfg = config.nocturne.wayland.editor;
  emacs-package =
    with pkgs;
    ((emacsPackagesFor emacs29-pgtk).emacsWithPackages (
      epkgs:
      let
        packageNames = mylib.filterStrList (str: !mylib.startsWithHash str) (
          mylib.removeEmpty (lib.strings.splitString "\n" (builtins.readFile ./packages.txt))
        );
      in
      (lib.foldl (acc: x: acc ++ [ epkgs."${x}" ]) [ ] packageNames)
      ++ [ epkgs.treesit-grammars.with-all-grammars ]
    ));
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
      nocturne.wayland.editor.exec = "${config.programs.emacs.package}/bin/emacsclient -c -a '${config.programs.emacs.package}/bin/emacs'";
      nocturne.wayland.editor.exec-reuse = "${config.programs.emacs.package}/bin/emacsclient -r";
      nocturne.wayland.editor.exec-start = "${config.programs.emacs.package}/bin/emacs --daemon";
    })
  ];
}
