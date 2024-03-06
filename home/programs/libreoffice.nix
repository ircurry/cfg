{ config, lib, pkgs, isNixos, ... }:

let
  cfg = config.nocturne.graphical.libreoffice;
in
{
  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      home.packages = with pkgs; [
        corefonts
        hunspell
        hunspellDicts.en_US
        libreoffice-qt
      ];
    })
    (lib.mkIf (cfg.enable && (isNixos == false)) {
      nixpkgs.config.allowUnfreePredicate = pkg:
        builtins.elem (lib.getName pkg) [
          "corefonts"
        ];
    })
  ];
}
