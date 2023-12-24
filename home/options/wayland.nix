{ lib, ... }: {
  options.nocturne.wayland = {
    # General info
    displays = {};
    dockDisplays = with lib; {
      enable = mkEnableOption "Enable docking";
    };
    keyboard = {};
    dockKeyboard = with lib; {
      enable = mkEnableOption "Enable secondary keyboard";
    };

    # Compositors
    compositor = lib.mkOption {
      type = lib.types.enum [ "hyprland" ];
      default = "hyprland";
      description = "Set default compositor";
    };
    hyprland = {};

    ## Bars
    #bar = {};
    #waybar = {};

    ## Application Launcher
    #appLauncher = {};
    #rofiWayland = {};
    #wofi = {};
    #fuzzel = {};

    ## Notifications
    #notificationDaemon = {};
    #mako = {};

    ## Terminals
    #terminal = {};

    ## Locking
    #screenLocker = {};
    #swaylock = {};
    #waylock = {};

    ## Wallpaper
    #paperDaemon = {};
    #swww = {};
    #swaybg = {};
  };
}
