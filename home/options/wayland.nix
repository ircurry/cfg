{ config, lib, ... }:
{
  options.nocturne.wayland = {
    # ===Abstract Options===
    browser = {
      name = lib.mkOption {
        type = lib.types.enum [ "firefox" ];
        default = "firefox";
        example = "firefox";
        description = "Name of default browser";
      };
    };
    compositor = {
      name = lib.mkOption {
        type = lib.types.enum [ "hyprland" ];
        default = "hyprland";
        example = "hyprland";
        description = "Name of the compositor";
      };
      profileExtra = lib.mkOption { type = lib.types.str; };
    };
    docked-monitors = lib.mkOption {
      type = lib.types.listOf (
        lib.types.submodule {
          options = {
            name = lib.mkOption { type = lib.types.str; };
            width = lib.mkOption { type = lib.types.int; };
            height = lib.mkOption { type = lib.types.int; };
            refreshRate = lib.mkOption { type = lib.types.int; };
            x = lib.mkOption { type = lib.types.int; };
            y = lib.mkOption { type = lib.types.int; };
            scale = lib.mkOption { type = lib.types.int; };
          };
        }
      );
    };
    editor = {
      name = lib.mkOption {
        type = lib.types.enum [ "emacs" ];
        default = "emacs";
        example = "emacs";
        description = "Name of the main system editor";
      };
      exec = lib.mkOption { type = lib.types.str; };
      exec-reuse = lib.mkOption { type = lib.types.nullOr lib.types.str; };
      exec-start = lib.mkOption { type = lib.types.nullOr lib.types.str; };
    };
    image = {
      name = lib.mkOption {
        type = lib.types.enum [ "imv" ];
        default = "imv";
        example = "imv";
        description = "Name of the main system image viewer";
      };
      exec = lib.mkOption { type = lib.types.str; };
      exec-dir = lib.mkOption { type = lib.types.str; };
    };
    lock = {
      name = lib.mkOption {
        type = lib.types.enum [ "swaylock" ];
        default = "swaylock";
        example = "swaylock";
        description = "Which screen locking program to use";
      };
      exec = lib.mkOption { type = lib.types.str; };
    };
    logout = {
      name = lib.mkOption {
        type = lib.types.enum [ "rofi-logout" ];
        default = "rofi-logout";
        example = "rofi-logout";
        description = "Which logout program to use";
      };
      exec = lib.mkOption { type = lib.types.str; };
    };
    menu = {
      name = lib.mkOption {
        type = lib.types.enum [ "rofi-wayland" ];
        default = "rofi-wayland";
        example = "rofi-wayland";
        description = "Which menu program to use";
      };
      drun = lib.mkOption { type = lib.types.str; };
      run = lib.mkOption { type = lib.types.str; };
      window = lib.mkOption { type = lib.types.str; };
    };
    monitors = lib.mkOption {
      type = lib.types.listOf (
        lib.types.submodule {
          options = {
            name = lib.mkOption { type = lib.types.str; };
            width = lib.mkOption { type = lib.types.int; };
            height = lib.mkOption { type = lib.types.int; };
            refreshRate = lib.mkOption { type = lib.types.int; };
            x = lib.mkOption { type = lib.types.int; };
            y = lib.mkOption { type = lib.types.int; };
            scale = lib.mkOption { type = lib.types.int; };
          };
        }
      );
    };
    notification = {
      daemon = lib.mkOption {
        type = lib.types.enum [ "mako" ];
        default = "mako";
        example = "mako";
        description = "Which notification daemon to use";
      };
      exec-start = lib.mkOption { type = lib.types.str; };
    };
    screenshot = {
      name = lib.mkOption {
        type = lib.types.nullOr (lib.types.enum [ "grim-slurp" ]);
        default = "grim-slurp";
        example = "grim-slurp";
        description = "Which screenshot program to use";
      };
      scrn = lib.mkOption { type = lib.types.package; };
      scrn-region = lib.mkOption { type = lib.types.package; };
    };
    terminal = {
      name = lib.mkOption {
        type = lib.types.enum [ "alacritty" ];
        default = "alacritty";
        example = "alacritty";
        description = "Which terminal emulator to use";
      };
      exec = lib.mkOption { type = lib.types.str; };
      exec-start = lib.mkOption { type = lib.types.nullOr lib.types.str; };
      exec-center = lib.mkOption { type = lib.types.str; };
    };

    # ===Program Options===
    hyprland = {
      col-active-border1 = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0D + "ee";
      };
      col-active-border2 = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0C + "ee";
      };
      col-inactive-border = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base03 + "aa";
      };
    };
    mako = {
      borderSize = lib.mkOption {
        type = lib.types.int;
        default = 2;
      };
      bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base02 + "AA";
      };
      fg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base05 + "FF";
      };
      border-color = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base03 + "FF";
      };
      progress-color = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0A + "FF";
      };
    };
    swaylock-effects = {
      bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base00;
      };
      fg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base05;
      };
      bg-inside = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base01;
      };
      ring = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base02;
      };
      key-press = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0D;
      };
      fg-ring-clear = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0A;
      };
      fg-ver = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base05;
      };
      ring-ver = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0D;
      };
      fg-wrong = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base08;
      };
      ring-wrong = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base08;
      };
      fg-caps = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base09;
      };
      key-press-caps = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base09;
      };
    };
    waybar = {
      stdMargin = lib.mkOption {
        type = lib.types.int;
        default = 4;
      };
      stdPadding = lib.mkOption {
        type = lib.types.int;
        default = 6;
      };
      stdFontSize = lib.mkOption {
        type = lib.types.int;
        default = 16;
      };
      stdIconSize = lib.mkOption {
        type = lib.types.int;
        default = 21;
      };
      default-bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base01;
      };
      default-fg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base01;
      };
      workspace-bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.wayland.waybar.default-bg;
      };
      workspace-hover-bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base00;
      };
      workspace-fg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base05;
      };
      workspace-empty = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base03;
      };
      workspace-urgent = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base09;
      };
      workspace-visible = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0A;
      };
      launcher-font-size = lib.mkOption {
        type = lib.types.int;
        default = 24;
      };
      launcher-bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0D;
      };
      launcher-fg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.wayland.waybar.default-bg;
      };
      battery-bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.wayland.waybar.default-bg;
      };
      battery-fg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0B;
      };
      battery-warning = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base09;
      };
      battery-critical = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base08;
      };
      clock-fg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base09;
      };
      clock-bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.wayland.waybar.default-bg;
      };
      tray-bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.wayland.waybar.default-bg;
      };
      backlight-fg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0A;
      };
      backlight-bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.wayland.waybar.default-bg;
      };
      audio-fg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0B;
      };
      audio-bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.wayland.waybar.default-bg;
      };
      network-fg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0A;
      };
      network-bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.wayland.waybar.default-bg;
      };
      network-disconnected = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base03;
      };
      power-font-size = lib.mkOption {
        type = lib.types.int;
        default = 24;
      };
      power-bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base08;
      };
      power-fg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.wayland.waybar.default-bg;
      };
      cpu-fg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0E;
      };
      cpu-bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.wayland.waybar.default-bg;
      };
      memory-fg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0D;
      };
      memory-bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.wayland.waybar.default-bg;
      };
      mpd-fg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0D;
      };
      mpd-bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.wayland.waybar.default-bg;
      };
      tooltip-fg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base05;
      };
      tooltip-bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.wayland.waybar.default-bg;
      };
    };
  };
}
