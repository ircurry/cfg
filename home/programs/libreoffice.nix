{
  config,
  lib,
  pkgs,
  pkgsUnfree,
  ...
}:

let
  cfg = config.nocturne.graphical.libreoffice;
in
{
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      pkgsUnfree.corefonts
      hunspell
      hunspellDicts.en_US
      libreoffice-qt
    ];
  };
}
