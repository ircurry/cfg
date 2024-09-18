{ ... }:

{
  # ===Import Home Configuration Modules===
  imports = [ ../../home ];

  # ===Nocturne Home Configuration===
  nocturne = {
    # themes.theme = "basic-dark";
    themes.theme = "catppuccin-mocha";
    # themes.theme = "flatwhite";
    # themes.theme = "grimshaw";
    # themes.theme = "gruvbox-dark";
    # themes.theme = "gruvbox-dark-bright";
    # themes.theme = "gruvbox-light";
    # themes.theme = "kanagawa-wave";
    # themes.theme = "nord-aurora";
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
      phetch.enable = true;
      shell.name = "fish";
      youtube.enable = true;
    };
    wayland = {
      hyprland.plugins.hyprbars = true;
      menu.name = "rofi";
      idleManager.name = null;
      monitors = [
        {
          name = "eDP-1";
          width = 2256;
          height = 1504;
          refreshRate = 60;
          x = 0;
          y = 0;
          scale = 2;
          state = "undocked";
        }
        {
          name = "DP-2";
          width = 1920;
          height = 1080;
          refreshRate = 60;
          x = 0;
          y = 0;
          scale = 1;
          state = "docked";
        }
      ];
    };
  };

  # ===Don't Change Please===
  home.stateVersion = "23.11"; # Please read the comment before changing.
}
