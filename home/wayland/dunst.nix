{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.nocturne.wayland.notification;
  frameColor = config.nocturne.wayland.dunst.frameColor + "ff";
  bg = config.nocturne.wayland.dunst.bg + "aa";
  fg = config.nocturne.wayland.dunst.fg + "ff";
  critical = config.nocturne.wayland.dunst.critical + "ff";
in
{
  config = lib.mkIf (cfg.daemon == "dunst") {
    home.packages = [
      pkgs.nerdfonts
      pkgs.libnotify
    ];
    services.dunst = {
      enable = true;
      iconTheme = {
        name = "hicolor";
        package = pkgs.hicolor-icon-theme;
        size = "32x32";
      };
      settings = {
        global = {
          monitor = 0;
          follow = "keyboard";
          enable_posix_regex = true;
          width = 300;
          height = 100;
          origin = "top-right";
          offset = "10x10";
          progress_bar = true;
          progress_bar_horizontal_alignment = "center";
          progress_bar_height = 10;
          progress_bar_corner_radius = 8;
          padding = 5;
          horizontal_padding = 5;
          gap_size = 10;
          font = "JetBrainsMono Nerd Font 10";
          stack_duplicates = true;
          corner_radius = 8;
          mouse_left_click = "close_current";
          mouse_middle_click = "close_all";
          mouse_right_click = "do_action, close_current";
          frame_width = 2;
          frame_color = "#${frameColor}";
        };
        urgency_low = {
          background = "#${bg}";
          foreground = "#${fg}";
          highlight = "#${frameColor}";
          timeout = 10;
        };
        urgency_normal = {
          background = "#${bg}";
          foreground = "#${fg}";
          highlight = "#${frameColor}";
          timeout = 10;
        };
        urgency_critical = {
          background = "#${bg}";
          foreground = "#${critical}";
          highlight = "#${critical}";
          frame_color = "#${critical}";
          timeout = 0;
        };
      };
    };
    nocturne.wayland.notification.exec-start = "${config.services.dunst.package}/bin/dunst";
  };
}
