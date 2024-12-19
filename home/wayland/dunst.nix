{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.nocturne.wayland.notification;
  frameColor = config.nocturne.wayland.dunst.frameColor + "ff";
  bg = config.nocturne.wayland.dunst.bg + config.nocturne.wayland.dunst.bg-opacity;
  fg = config.nocturne.wayland.dunst.fg + "ff";
  critical = config.nocturne.wayland.dunst.critical + "ff";
  increaseColor = config.nocturne.wayland.dunst.increaseColor;
  decreaseColor = config.nocturne.wayland.dunst.decreaseColor;
  mutedColor = config.nocturne.wayland.dunst.mutedColor;

  volup = pkgs.writeShellApplication {
    name = "volup";
    runtimeInputs = with pkgs; [
      config.services.dunst.package
      pamixer
    ];
    text = ''
        pamixer -i 1
        if [ "$(pamixer --get-mute)" = "true" ]; then
            FG_COLOR="#${mutedColor}"
            MUTED_STRING="(muted) "
        else
            FG_COLOR="#${increaseColor}"
            MUTED_STRING=""
        fi
        VOLUME="$(pamixer --get-volume)"
      	dunstify -u low "Volume: ''${MUTED_STRING}''${VOLUME}%" -h int:value:"$VOLUME" -h string:fgcolor:"$FG_COLOR" -r 9993 -t 2000
    '';
  };
  voldown = pkgs.writeShellApplication {
    name = "voldown";
    runtimeInputs = with pkgs; [
      config.services.dunst.package
      pamixer
    ];
    text = ''
        pamixer -d 1
        if [ "$(pamixer --get-mute)" = "true" ]; then
            FG_COLOR="#${mutedColor}"
            MUTED_STRING="(muted) "
        else
            FG_COLOR="#${decreaseColor}"
            MUTED_STRING=""
        fi
        VOLUME="$(pamixer --get-volume)"
      	dunstify -u low "Volume: ''${MUTED_STRING}''${VOLUME}%" -h int:value:"$VOLUME" -h string:fgcolor:"$FG_COLOR" -r 9993 -t 2000
    '';
  };
  volmute = pkgs.writeShellApplication {
    name = "volmute";
    runtimeInputs = with pkgs; [
      config.services.dunst.package
      pamixer
    ];
    text = ''
      pamixer -t
      FG_COLOR="#${mutedColor}"
      VOLUME="$(pamixer --get-volume)"
      if [ "$(pamixer --get-mute)" = "true" ]; then
          MUTED_STRING="Muted"
      else
          MUTED_STRING="Unmuted"
      fi
      dunstify -u low "Volume: ''${MUTED_STRING}" -h int:value:"$VOLUME" -h string:fgcolor:"$FG_COLOR" -r 9993 -t 2000
    '';
  };
  brightup = pkgs.writeShellApplication {
    name = "brightup";
    runtimeInputs = with pkgs; [
      config.services.dunst.package
      brightnessctl
      bc
    ];
    text = ''
      brightnessctl s +1%
      FG_COLOR="#${increaseColor}"
      BRIGHTNESS="$(echo "scale = 2; ($(brightnessctl get)/$(brightnessctl max)) * 100" | bc | sed 's/\..*//g')"
      dunstify -u low "Brightness: ''${BRIGHTNESS}%" -h int:value:"$BRIGHTNESS" -h string:fgcolor:"$FG_COLOR" -r 9993 -t 2000
    '';
  };
  brightdown = pkgs.writeShellApplication {
    name = "brightdown";
    runtimeInputs = with pkgs; [
      config.services.dunst.package
      brightnessctl
      bc
    ];
    text = ''
      brightnessctl s 1%-
      FG_COLOR="#${decreaseColor}"
      BRIGHTNESS="$(echo "scale = 2; ($(brightnessctl get)/$(brightnessctl max)) * 100" | bc | sed 's/\..*//g')"
      dunstify -u low "Brightness: ''${BRIGHTNESS}%" -h int:value:"$BRIGHTNESS" -h string:fgcolor:"$FG_COLOR" -r 9993 -t 2000
    '';
  };
in
{
  config = lib.mkIf (cfg.daemon == "dunst") {
    home.packages = [
      volup
      voldown
      volmute
      brightup
      brightdown
      pkgs.nerd-fonts.jetbrains-mono
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
          height = "(0, 100)";
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
    nocturne.wayland.notification.exec-volup = "${lib.getExe volup}";
    nocturne.wayland.notification.exec-voldown = "${lib.getExe voldown}";
    nocturne.wayland.notification.exec-volmute = "${lib.getExe volmute}";
    nocturne.wayland.notification.exec-brightup = "${lib.getExe brightup}";
    nocturne.wayland.notification.exec-brightdown = "${lib.getExe brightdown}";
  };
}
