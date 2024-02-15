{ config, lib, ... }: {
  options.nocturne.wayland = {
    editor = {
      name = lib.mkOption {
        type = lib.types.enum [ "emacs" ];
        default = "emacs";
        example = "emacs";
        description = "Name of the main system editor";
      };
      exec = lib.mkOption {
        type = lib.types.str;
      };
      exec-reuse = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
      };
      exec-start = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
      };
    };
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
    image = {
      name = lib.mkOption {
        type = lib.types.enum [ "imv" ];
        default = "imv";
        example = "imv";
        description = "Name of the main system image viewer";
      };
      exec = lib.mkOption {
        type = lib.types.str;
      };
      exec-dir = lib.mkOption {
        type = lib.types.str;
      };
    };
    lock = {
      name = lib.mkOption {
        type = lib.types.enum [ "swaylock" ];
        default = "swaylock";
        example = "swaylock";
        description = "Which screen locking program to use";
      };
      exec = lib.mkOption {
        type = lib.types.str;
      };
    };
    logout = {
      name = lib.mkOption {
        type = lib.types.enum [ "rofi-logout" ];
        default = "rofi-logout";
        example = "rofi-logout";
        description = "Which logout program to use";
      };
      exec = lib.mkOption {
        type = lib.types.str;
      };
    };
    menu = {
      name = lib.mkOption {
        type = lib.types.enum [ "rofi-wayland" ];
        default = "rofi-wayland";
        example = "rofi-wayland";
        description = "Which menu program to use";
      };
      drun = lib.mkOption {
        type = lib.types.str;
      };
      run = lib.mkOption {
        type = lib.types.str;
      };
      window = lib.mkOption {
        type = lib.types.str;
      };
    };
    monitors = lib.mkOption {
      type = lib.types.listOf (lib.types.submodule {
        options = {
          name = lib.mkOption {
            type = lib.types.str;
          };
          width = lib.mkOption {
            type = lib.types.int;
          };
          height = lib.mkOption {
            type = lib.types.int;
          };
          refreshRate = lib.mkOption {
            type = lib.types.int;
          };
          x = lib.mkOption {
            type = lib.types.int;
          };
          y = lib.mkOption {
            type = lib.types.int;
          };
          scale = lib.mkOption {
            type = lib.types.int;
          };
        };
      });
    };
    docked-monitors = lib.mkOption {
      type = lib.types.listOf (lib.types.submodule {
        options = {
          name = lib.mkOption {
            type = lib.types.str;
          };
          width = lib.mkOption {
            type = lib.types.int;
          };
          height = lib.mkOption {
            type = lib.types.int;
          };
          refreshRate = lib.mkOption {
            type = lib.types.int;
          };
          x = lib.mkOption {
            type = lib.types.int;
          };
          y = lib.mkOption {
            type = lib.types.int;
          };
          scale = lib.mkOption {
            type = lib.types.int;
          };
        };
      });
    };
    notification = {
      daemon = lib.mkOption {
        type = lib.types.enum [ "mako" ];
        default = "mako";
        example = "mako";
        description = "Which notification daemon to use";
      };
      exec-start = lib.mkOption{
        type = lib.types.str;
      };
    };
    screenshot = {
      name = lib.mkOption {
        type = lib.types.nullOr (lib.types.enum [ "grim-slurp" ]);
        default = "grim-slurp";
        example = "grim-slurp";
        description = "Which screenshot program to use";
      };
      scrn = lib.mkOption {
        type = lib.types.package;
      };
      scrn-region = lib.mkOption {
        type = lib.types.package;
      };
    };
    terminal = {
      name = lib.mkOption {
        type = lib.types.nullOr (lib.types.enum [ "alacritty" ]);
        default = "alacritty";
        example = "alacritty";
        description = "Which terminal emulator to use";
      };
      exec = lib.mkOption{
        type = lib.types.str;
      };
      exec-start = lib.mkOption{
        type = lib.types.nullOr lib.types.str;
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
      workspace-bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base01;
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
        default = config.nocturne.themes.colors.base01;
      };
      battery-bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base01;
      };
      battery-fg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0B;
      };
      battery-warning = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0A;
      };
      battery-critical = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base08;
      };
      clock-fg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base05;
      };
      clock-bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base01;
      };
      tray-bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base01;
      };
      backlight-fg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0A;
      };
      backlight-bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base01;
      };
      audio-fg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0D;
      };
      audio-bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base01;
      };
      network-fg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base05;
      };
      network-bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base01;
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
        default = config.nocturne.themes.colors.base01;
      };
    };
  };
}
