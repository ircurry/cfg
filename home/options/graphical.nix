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
