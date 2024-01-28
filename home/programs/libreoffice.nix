{ config, lib, pkgs, ... }:

let
  cfg = config.nocturne.graphical.libreoffice;
in
{
  config = lib.mkIf cfg.enable {
    nixpkgs.config.allowUnfreePredicate = pkg:
      builtins.elem (lib.getName pkg) [
        "corefonts"
      ];
    home.packages = with pkgs; [
      corefonts
      hunspell
      hunspellDicts.en_US
      libreoffice-qt
    ];
  };
}
