{ config, lib, ... }:
{
  options.nocturne.wayland = {
    # ===Abstract Options===
    bar = {
      name = lib.mkOption {
        type = lib.types.enum [
          "waybar"
          "ags"
          "ags-side"
        ];
        default = "waybar";
        example = "ags-top";
        description = "Name of bar use by the Wayland Compositor";
      };
      exec-on = lib.mkOption { type = lib.types.str; };
      exec-off = lib.mkOption { type = lib.types.str; };
      exec-start = lib.mkOption { type = lib.types.str; };
      exec-toggle = lib.mkOption { type = lib.types.str; };
    };
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
        type = lib.types.nullOr (lib.types.enum [ "hyprland" ]);
        default = "hyprland";
        example = "hyprland";
        description = "Name of the compositor";
      };
      profileExtra = lib.mkOption { type = lib.types.str; };
    };
    decoration = {
      stdgaps = lib.mkOption {
        type = lib.types.addCheck lib.types.int (x: x >= 0 && (0 == (lib.mod x 2)));
        default = 6;
        example = "12";
        description = "The standard gapping between objects such as windows";
      };
      stdborderthick = lib.mkOption {
        type = lib.types.ints.positive;
        default = 3;
        example = "2";
        description = "The standard border thickness";
      };
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
    fileManager = {
      name = lib.mkOption {
        type = lib.types.enum [ "thunar" ];
        default = "thunar";
        example = "thunar";
        description = "Name of the main GUI file manager";
      };
    };
    idleManager = {
      name = lib.mkOption {
        type = lib.types.nullOr (lib.types.enum [ "swayidle" ]);
        default = "swayidle";
        example = "swayidle";
        description = "Which idle manager to use, if any";
      };
      exec = lib.mkOption {
        type = lib.types.str;
        description = "Command to execute idle manager";
      };
    };
    image = {
      name = lib.mkOption {
        type = lib.types.enum [
          "swayimg"
          "imv"
        ];
        default = "swayimg";
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
    menu = {
      name = lib.mkOption {
        type = lib.types.enum [
          "fuzzel"
          "rofi"
        ];
        default = "fuzzel";
        example = "rofi";
        description = "Which menu program to use";
      };
      promptSwitch = lib.mkOption { type = lib.types.str; };
      exec = lib.mkOption { type = lib.types.str; };
      exec-run = lib.mkOption { type = lib.types.str; };
      exec-dmenu = lib.mkOption { type = lib.types.str; };
      exec-logout = lib.mkOption { type = lib.types.str; };
    };
    monitor-profiles =
      with lib.types;
      let
        inherit (lib) mkOption;
        resolution = mkOption {
          default = null;
          type = nullOr (submodule {
            options = {
              width = mkOption { type = ints.u32; };
              height = mkOption { type = ints.u32; };
              refresh_rate = mkOption { type = ints.positive; };
            };
          });
        };
        position = mkOption {
          default = null;
          type = nullOr (submodule {
            options = {
              x = mkOption { type = int; };
              y = mkOption { type = int; };
            };
          });
        };
        monitors = mkOption {
          type = listOf (submodule {
            options = {
              name = mkOption {
                type = nullOr str;
                default = null;
              };
              inherit resolution position;
              scale = mkOption {
                type = nullOr (numbers.between 1 256);
                default = null;
              };
              enabled = mkOption {
                type = bool;
                default = true;
              };
            };
          });
        };
      in
      mkOption {
        type = listOf (submodule {
          options = {
            name = mkOption { type = str; };
            inherit monitors;
          };
        });
        default = [
          {
            name = "default";
            monitors = [
              {
                enabled = true;
              }
            ];
          }
        ];
      };
    notification = {
      daemon = lib.mkOption {
        type = lib.types.enum [
          "dunst"
          "mako"
        ];
        default = "dunst";
        example = "mako";
        description = "Which notification daemon to use";
      };
      exec-start = lib.mkOption { type = lib.types.str; };
      exec-volup = lib.mkOption { type = lib.types.str; };
      exec-voldown = lib.mkOption { type = lib.types.str; };
      exec-volmute = lib.mkOption { type = lib.types.str; };
      exec-brightup = lib.mkOption { type = lib.types.str; };
      exec-brightdown = lib.mkOption { type = lib.types.str; };
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
    dunst = {
      paddingMax = lib.mkOption {
        type = lib.types.ints.positive;
        default = 8;
      };
      paddingMin = lib.mkOption {
        type = lib.types.ints.positive;
        default = 4;
      };
      padding =
        let
          cfg = config.nocturne.wayland.dunst;
          max = cfg.paddingMax;
          min = cfg.paddingMin;
          inherit (config.nocturne.wayland.decoration) stdgaps;
          default = (if stdgaps > max then max else (if stdgaps < min then min else stdgaps));
        in
        lib.mkOption {
          type = lib.types.addCheck lib.types.int (x: x <= max && x >= min);
          inherit default;
        };
      offset =
        let
          deco = config.nocturne.wayland.decoration;
          val = deco.stdgaps + deco.stdborderthick + (deco.stdgaps / 2);
        in
        lib.mkOption {
          type = lib.types.str;
          default = "${builtins.toString val}x${builtins.toString val}";
        };
      borderthick = lib.mkOption {
        type = lib.types.ints.positive;
        default = config.nocturne.wayland.decoration.stdborderthick;
      };
      frameColor = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0D;
      };
      bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base02;
      };
      bg-opacity = lib.mkOption {
        type = lib.types.str;
        default = "aa";
      };
      fg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base05;
      };
      critical = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base08;
      };
      increaseColor = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0B;
      };
      decreaseColor = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0A;
      };
      mutedColor = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0D;
      };
    };
    fuzzel = {
      background = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base00;
      };
      text = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base05;
      };
      match = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base08;
      };
      selection = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base01;
      };
      selection-text = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.wayland.fuzzel.text;
      };
      selection-match = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.wayland.fuzzel.match;
      };
      border = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0D;
      };
    };
    hyprland = {
      plugins.hyprbars.enable = lib.mkEnableOption "Whether to enable window title bars in Hyprland";
      col-active-border1 = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0D + "ee";
      };
      col-active-border2 = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base0C + "ee";
      };
      col-background = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base00 + "ff";
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
        default = config.nocturne.themes.colors.base0D + "FF";
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
      stdSpacingMax = lib.mkOption {
        type = lib.types.ints.positive;
        default = 8;
      };
      stdSpacingMin = lib.mkOption {
        type = lib.types.ints.positive;
        default = 4;
      };
      stdSpacing =
        let
          cfg = config.nocturne.wayland.waybar;
          max = cfg.stdSpacingMax;
          min = cfg.stdSpacingMin;
          inherit (config.nocturne.wayland.decoration) stdgaps;
          default = (if stdgaps > max then max else (if stdgaps < min then min else stdgaps));
        in
        lib.mkOption {
          type = lib.types.addCheck lib.types.int (x: x <= max && x >= min);
          inherit default;
        };
      stdPaddingMax = lib.mkOption {
        type = lib.types.ints.positive;
        default = 8;
      };
      stdPaddingMin = lib.mkOption {
        type = lib.types.ints.positive;
        default = 4;
      };
      stdPadding =
        let
          cfg = config.nocturne.wayland.waybar;
          max = cfg.stdPaddingMax;
          min = cfg.stdPaddingMin;
          inherit (config.nocturne.wayland.decoration) stdgaps;
          default = (if stdgaps > max then max else (if stdgaps < min then min else stdgaps));
        in
        lib.mkOption {
          type = lib.types.addCheck lib.types.int (x: x <= max && x >= min);
          inherit default;
        };
      workSpacePaddingMax = lib.mkOption {
        type = lib.types.ints.positive;
        default = 10;
      };
      workSpacePaddingMin = lib.mkOption {
        type = lib.types.ints.positive;
        default = 4;
      };
      workSpacePadding =
        let
          cfg = config.nocturne.wayland.waybar;
          max = cfg.workSpacePaddingMax;
          min = cfg.workSpacePaddingMin;
          val = cfg.stdPadding * 2;
          default = (if val > max then max else (if val < min then min else val));
        in
        lib.mkOption {
          type = lib.types.addCheck lib.types.int (x: x <= max && x >= min);
          inherit default;
        };
      stdFontSize = lib.mkOption {
        type = lib.types.int;
        default = 16;
      };
      stdIconSize = lib.mkOption {
        type = lib.types.int;
        default = 21;
      };
      simple = {
        default-bg = lib.mkOption {
          type = lib.types.str;
          default = config.nocturne.themes.colors.base01;
        };
        default-fg = lib.mkOption {
          type = lib.types.str;
          default = config.nocturne.themes.colors.base05;
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
          default = config.nocturne.themes.colors.base0F;
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
          default = config.nocturne.themes.colors.base0D;
        };
        audio-bg = lib.mkOption {
          type = lib.types.str;
          default = config.nocturne.wayland.waybar.default-bg;
        };
        network-fg = lib.mkOption {
          type = lib.types.str;
          default = config.nocturne.themes.colors.base0B;
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
          default = config.nocturne.themes.colors.base08;
        };
        cpu-bg = lib.mkOption {
          type = lib.types.str;
          default = config.nocturne.wayland.waybar.default-bg;
        };
        memory-fg = lib.mkOption {
          type = lib.types.str;
          default = config.nocturne.themes.colors.base09;
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
      workspace-fg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base05;
      };
      workspace-hover-bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base00;
      };
      workspace-empty-bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.wayland.waybar.workspace-bg;
      };
      workspace-empty = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base03;
      };
      workspace-urgent-bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.wayland.waybar.workspace-bg;
      };
      workspace-urgent = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base09;
      };
      workspace-active-bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.wayland.waybar.workspace-bg;
      };
      workspace-active = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.wayland.waybar.workspace-fg;
      };
      workspace-visible-bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.wayland.waybar.workspace-bg;
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
      battery-warning-bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.wayland.waybar.default-bg;
      };
      battery-warning = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.themes.colors.base09;
      };
      battery-critical-bg = lib.mkOption {
        type = lib.types.str;
        default = config.nocturne.wayland.waybar.default-bg;
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
