{ config, pkgs, inputs, ... }:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "recur";
  home.homeDirectory = "/home/recur";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.11"; # Please read the comment before changing.

  imports = [ 
    ../../home
  ];

  home.packages = with pkgs; [
    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    mpv

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  sops = {
    defaultSopsFile = ../../secrets/secrets.yaml;
    defaultSopsFormat = "yaml";
    age.keyFile = "/home/recur/.config/sops/age/keys.txt";
  };
  
  # The Meat of my configuration
  nocturne = {
    #themes.theme = "basic-dark";
    themes.theme = "catppuccin-mocha";
    #themes.theme = "gruvbox-dark-medium";
    #themes.theme = "nord-aurora";
    graphical = {
      alacritty.enable = true;
      brave.enable = false;
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
      file-convert.enable = true;
      phetch.enable = true;
      scripts = {
        youtubeScripts.enable = true;
      };
      ytfzf.enable = true;
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
}
