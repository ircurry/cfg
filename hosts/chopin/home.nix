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
    hello
    kate
    lf
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

  # The Meat of my configuration
  nocturne = {
    editors.emacs = {
      enable = true;
    };
    graphical = {
      alacritty.enable = true;
      brave.enable = false;
      firefox.enable = true;
      flatpak.enable = false;
      keepassxc.enable = true;
      kid3.enable = true;
      obs.enable = true;
      signalDesktop.enable = false;
      torBrowser.enable = true;
    };
    cli = {
      amfora.enable = true;
      scripts = {
        youtubeScripts.enable = true;
      };
    };
  };
}
