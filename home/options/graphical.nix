{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.nocturne.graphical = {
    # ===Program Options===
    alacritty = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Enable Alacritty";
      };
      bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base00;
      };
      fg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base05;
      };
      black = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base01;
      };
      red = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base08;
      };
      yellow = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0A;
      };
      green = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0B;
      };
      cyan = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0C;
      };
      blue = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0D;
      };
      magenta = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0E;
      };
      white = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base06;
      };
      bright-black = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base03;
      };
      bright-red = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base08;
      };
      bright-yellow = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0A;
      };
      bright-green = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0B;
      };
      bright-cyan = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0C;
      };
      bright-blue = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0D;
      };
      bright-magenta = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0E;
      };
      bright-white = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base05;
      };
    };
    anki.enable = lib.mkEnableOption "Enable Anki Flash Card Program";
    brave.enable = lib.mkEnableOption "Enable Brave Browser";
    discord.enable = lib.mkEnableOption "Enable Vesktop Discord Client";
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
    keymapp.enable = lib.mkEnableOption "Enable ZSA's Keymapp";
    kid3.enable = lib.mkEnableOption "Enable kid3";
    libreoffice.enable = lib.mkEnableOption "Enable LibreOffice";
    mullvadBrowser.enable = lib.mkEnableOption "Enable Mullvad Browser";
    mullvad-vpn.enable = lib.mkEnableOption "Enable Mullvad VPN";
    obs.enable = lib.mkEnableOption "Enable OBS";
    # TODO: DEPRICATED: will be moved to wayland option attr set
    rofi = {
      enable = lib.mkEnableOption "Enable Rofi";
      bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base00;
      };
      bg-selection = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base01;
      };
      fg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base05;
      };
      fg-selection = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base05;
      };
      fg-placeholder = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base03;
      };
      fg-urgent = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base09;
      };
      fg-active = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0A;
      };
      border-color = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0D;
      };
    };
    signalDesktop.enable = lib.mkEnableOption "Enable Signal Desktop";
    thunderbird.enable = lib.mkEnableOption "Enable Thunderbird";
    torBrowser.enable = lib.mkEnableOption "Enable the Tor Browser";
    veracrypt.enable = lib.mkEnableOption "Enable VeraCrypt";
    zathura.enable = lib.mkEnableOption "Enable Zathura";
  };
}
