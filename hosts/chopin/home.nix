{ ... }:

{
  # ===Import Home Configuration Modules===
  imports = [ ../../home ];

  # ===Nocturne Home Configuration===
  nocturne = {
    # themes.theme = "basic-dark";
    # themes.theme = "catppuccin-mocha";
    # themes.theme = "flatwhite";
    # themes.theme = "grimshaw";
    # themes.theme = "gruvbox-dark";
    # themes.theme = "gruvbox-dark-bright";
    # themes.theme = "gruvbox-light";
    # themes.theme = "kanagawa-wave";
    themes.theme = "nord-aurora";
    # themes.theme = "nord-light";
    # themes.theme = "oceanic-next";
    # themes.theme = "tomorrow-night";
    graphical = {
      alacritty.enable = true;
      anki.enable = true;
      brave.enable = false;
      discord.enable = true;
      emacs.enable = true;
      firefox.enable = true;
      flatpak.enable = false;
      imv.enable = true;
      keepassxc.enable = true;
      kid3.enable = true;
      libreoffice.enable = true;
      mullvadBrowser.enable = true;
      obs.enable = true;
      signalDesktop.enable = true;
      thunderbird.enable = true;
      torBrowser.enable = true;
      veracrypt.enable = true;
      zathura.enable = true;
    };
    cli = {
      age.enable = true;
      amfora.enable = true;
      ani-cli.enable = true;
      direnv.enable = true;
      file-convert.enable = true;
      mpd.enable = true;
      newsboat.enable = true;
      phetch.enable = true;
      shell.name = "fish";
      youtube.enable = true;
    };
    wayland = {
      hyprland.plugins.hyprbars = false;
      menu.name = "rofi";
      idleManager.name = null;
      monitor-profiles = [
        {
          name = "undocked";
          monitors = [
            {
              name = "eDP-1";
              resolution = {
                width = 2256;
                height = 1504;
                refresh_rate = 60;
              };
              position = {
                x = 0;
                y = 0;
              };
              scale = 2;
              enabled = true;
            }
            {
              name = "DP-2";
              resolution = {
                width = 1920;
                height = 1080;
                refresh_rate = 60;
              };
              position = {
                x = 0;
                y = 1504;
              };
              scale = 1.00001;
              enabled = true;
            }
            {
              enabled = true;
            }
          ];
        }
        {
          name = "docked";
          monitors = [
            {
              name = "eDP-1";
              enabled = false;
            }
            {
              name = "DP-2";
              resolution = {
                width = 1920;
                height = 1080;
                refresh_rate = 60;
              };
              position = {
                x = 0;
                y = 0;
              };
              scale = 1;
              enabled = true;
            }
            {
              enabled = true;
            }
          ];
        }
      ];
    };
  };

  # ===Don't Change Please===
  home.stateVersion = "23.11"; # Please read the comment before changing.
}
