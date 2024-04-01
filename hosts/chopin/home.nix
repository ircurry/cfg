{
  config,
  pkgs,
  inputs,
  ...
}:

{
  # ===Import Home Configuration Modules===
  imports = [ ../../home ];

  # ===Nocturne Home Configuration===
  nocturne = {
    #themes.theme = "basic-dark";
    themes.theme = "catppuccin-mocha";
    #themes.theme = "gruvbox-dark";
    #themes.theme = "gruvbox-dark-bright";
    #themes.theme = "gruvbox-light";
    #themes.theme = "kanagawa-wave";
    #themes.theme = "nord-aurora";
    #themes.theme = "nord-light";
    graphical = {
      alacritty.enable = true;
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
      zathura.enable = true;
    };
    cli = {
      amfora.enable = true;
      ani-cli.enable = true;
      conversion.enable = true;
      direnv.enable = true;
      file-convert.enable = true;
      mpd.enable = true;
      phetch.enable = true;
      shell.name = "fish";
      youtube.enable = true;
    };
    wayland.monitors = [
      {
        name = "eDP-1";
        width = 2256;
        height = 1504;
        refreshRate = 60;
        x = 0;
        y = 0;
        scale = 2;
      }
    ];
    wayland.docked-monitors = [
      {
        name = "DP-2";
        width = 1920;
        height = 1080;
        refreshRate = 60;
        x = 0;
        y = 0;
        scale = 1;
      }
    ];
  };

  # ===Don't Change Please===
  home.stateVersion = "23.11"; # Please read the comment before changing.
}
