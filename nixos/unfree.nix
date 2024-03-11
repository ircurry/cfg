{ config, lib, pkgs, ... }:
let
  # ===Nixpkgs Unfree Configuration===
  nixpkgs-cfg = config.nixpkgs.config.allowUnfree;

  # ===User Application Configuration===
  libreoffice-cfg = config.hm.nocturne.graphical.libreoffice;

  # ===Unfree Package Lists===
  unfree-pkgs = [ ] ++ lib.optionals (libreoffice-cfg.enable) [ "corefonts" ];
in {
  config = {
    nixpkgs.config.allowUnfreePredicate = pkg:
      builtins.elem (lib.getName pkg) unfree-pkgs;
  };
}
