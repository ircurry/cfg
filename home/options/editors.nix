{ lib, pkgs, ... }: {
  options.nocturne.editors = {
    main = lib.mkOption {
      type = lib.types.enum [ "emacs" "vim" ];
      default = "emacs";
      example = "emacs";
      description = "Name of the main system editor";
    };
    emacs = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Enable Emacs";
      };
      pkg = lib.mkOption {
        type = lib.types.package;
        default = pkgs.emacs29-pgtk;
        example = pkgs.emacs29-pgtk;
        description = "Emacs package to use";
      };
      associatedPkgs = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = with pkgs; [ nerdfonts texliveFull ]; 
        example = with pkgs; [ nerdfonts texliveFull ];
        description = "Nix packages that emacs uses";
      };
    };
  };
}
