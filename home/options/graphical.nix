{ lib, ... }: {
  options.nocturne.graphical = {
    alacritty.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable Alacritty";
    };
    brave.enable = lib.mkEnableOption "Enable Brave Browser";
    firefox.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable Firefox";
    };
    flatpak.enable = lib.mkEnableOption "Enable Flatpaks";
    keepassxc.enable = lib.mkEnableOption "Enable KeepassXC";
    kid3.enable = lib.mkEnableOption "Enable kid3";
    obs.enable = lib.mkEnableOption "Enable OBS";
    signalDesktop.enable = lib.mkEnableOption "Enable Signal Desktop";
    torBrowser.enable = lib.mkEnableOption "Enable the Tor Browser";
  };
}
