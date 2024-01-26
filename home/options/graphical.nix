{ lib, pkgs, ... }: {
  options.nocturne.graphical = {
    alacritty.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable Alacritty";
    };
    brave.enable = lib.mkEnableOption "Enable Brave Browser";
    emacs = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Enable Emacs";
      };
    };
    firefox.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable Firefox";
    };
    flatpak.enable = lib.mkEnableOption "Enable Flatpaks";
    imv.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable imv";
    };
    keepassxc.enable = lib.mkEnableOption "Enable KeepassXC";
    kid3.enable = lib.mkEnableOption "Enable kid3";
    libreoffice.enable = lib.mkEnableOption "Enable LibreOffice";
    mullvadBrowser.enable = lib.mkEnableOption "Enable Mullvad Browser";
    obs.enable = lib.mkEnableOption "Enable OBS";
    signalDesktop.enable = lib.mkEnableOption "Enable Signal Desktop";
    thunderbird.enable = lib.mkEnableOption "Enable Thunderbird";
    torBrowser.enable = lib.mkEnableOption "Enable the Tor Browser";
    zathura.enable = lib.mkEnableOption "Enable Zathura";
  };
}
