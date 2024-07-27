{
  config,
  lib,
  pkgs,
  ...
}:
let
  uiStyle = config.nocturne.wayland.uiStyle;
in
{
  config = lib.mkMerge [
    {
      home.packages = with pkgs; [
        material-design-icons
        nerdfonts
      ];
    }
    (lib.mkIf (uiStyle == "simple") {
      programs.waybar = {
        enable = true;
        package = pkgs.waybar;
        style =
          let
            stdMargin = builtins.toString config.nocturne.wayland.waybar.stdMargin;
            stdPadding = builtins.toString config.nocturne.wayland.waybar.stdPadding;
            stdFontSize = builtins.toString config.nocturne.wayland.waybar.stdFontSize;
            default-bg = config.nocturne.wayland.waybar.simple.default-bg;
            default-fg = config.nocturne.wayland.waybar.simple.default-fg;
            workspace-bg = config.nocturne.wayland.waybar.simple.workspace-bg;
            workspace-hover-bg = config.nocturne.wayland.waybar.simple.workspace-hover-bg;
            workspace-fg = config.nocturne.wayland.waybar.simple.workspace-fg;
            workspace-empty = config.nocturne.wayland.waybar.simple.workspace-empty;
            workspace-urgent = config.nocturne.wayland.waybar.simple.workspace-urgent;
            workspace-visible = config.nocturne.wayland.waybar.simple.workspace-visible;
            launcher-font-size = builtins.toString config.nocturne.wayland.waybar.simple.launcher-font-size;
            launcher-fg = config.nocturne.wayland.waybar.simple.launcher-fg;
            launcher-bg = config.nocturne.wayland.waybar.simple.launcher-bg;
            battery-fg = config.nocturne.wayland.waybar.simple.battery-fg;
            battery-bg = config.nocturne.wayland.waybar.simple.battery-bg;
            battery-warning = config.nocturne.wayland.waybar.simple.battery-warning;
            battery-critical = config.nocturne.wayland.waybar.simple.battery-critical;
            clock-fg = config.nocturne.wayland.waybar.simple.clock-fg;
            clock-bg = config.nocturne.wayland.waybar.simple.clock-bg;
            tray-bg = config.nocturne.wayland.waybar.simple.tray-bg;
            backlight-fg = config.nocturne.wayland.waybar.simple.backlight-fg;
            backlight-bg = config.nocturne.wayland.waybar.simple.backlight-bg;
            audio-fg = config.nocturne.wayland.waybar.simple.audio-fg;
            audio-bg = config.nocturne.wayland.waybar.simple.audio-bg;
            network-fg = config.nocturne.wayland.waybar.simple.network-fg;
            network-bg = config.nocturne.wayland.waybar.simple.network-bg;
            network-disconnected = config.nocturne.wayland.waybar.simple.network-disconnected;
            power-font-size = builtins.toString config.nocturne.wayland.waybar.simple.power-font-size;
            power-fg = config.nocturne.wayland.waybar.simple.power-fg;
            power-bg = config.nocturne.wayland.waybar.simple.power-bg;
            cpu-fg = config.nocturne.wayland.waybar.simple.cpu-fg;
            cpu-bg = config.nocturne.wayland.waybar.simple.cpu-bg;
            memory-fg = config.nocturne.wayland.waybar.simple.memory-fg;
            memory-bg = config.nocturne.wayland.waybar.simple.memory-bg;
            mpd-fg = config.nocturne.wayland.waybar.simple.mpd-fg;
            mpd-bg = config.nocturne.wayland.waybar.simple.mpd-bg;
            tooltip-bg = config.nocturne.wayland.waybar.simple.tooltip-bg;
            tooltip-fg = config.nocturne.wayland.waybar.simple.tooltip-fg;
          in
          ''
            * {
              border: none;
              border-radius: 0px;
            }
            tooltip {
              font-family: Material Design Icons, Iosevka Nerd Font Mono, DejaVu Sans;
              box-shadow: none;
              text-shadow: none;
              background: none;
              background-color: #${tooltip-bg};
              font-size: ${stdFontSize}px;
            }
            tooltip * {
              font-family: Material Design Icons, Iosevka Nerd Font Mono, DejaVu Sans;
              box-shadow: none;
              text-shadow: none;
              background: none;
              color: #${tooltip-fg};
              font-size: ${stdFontSize}px;
              padding: 0px 0px;
            }
            window#waybar {
              background-color: #${default-bg};
              color: #${default-fg};
            }
            /* ===Workspaces=== */
            #workspaces {
              background-color: #${workspace-bg};
              margin: 0px 0px;
              padding: 0px 0px;
            }
            #workspaces button {
              color: #${workspace-fg};
              font-family: Material Design Icons, Iosevka Nerd Font Mono, DejaVu Sans;
              font-size: ${stdFontSize}px;
              margin: 0px;
              padding: 0px ${stdPadding}px;
            }
            #workspaces button:hover {
              box-shadow: none;
              text-shadow: none;
              background: none;
              background-color: #${workspace-hover-bg};
            }
            #workspaces button.empty {
              color: #${workspace-empty};
            }
            #workspaces button.active {
              color: #${workspace-fg};
            }
            #workspaces button.urgent {
              color: #${workspace-urgent};
            }
            #workspaces button.visible {
              color: #${workspace-bg};
              background-color: #${default-fg};
            }
            /* ===Launcher=== */
            #custom-launcher {
              font-family: Material Design Icons;
              font-size: ${launcher-font-size}px;
              color: #${launcher-fg};
              background-color: #${launcher-bg};
              margin: 0px 0px;
              padding: 0px ${stdPadding}px;
            }
            /* ===Battery=== */
            #battery {
              font-family: Material Design Icons, Iosevka Nerd Font Mono;
              font-size: ${stdFontSize}px;
              margin: 0px 0px;
              padding: 0px ${stdPadding}px;
              color: #${battery-fg};
              background-color: #${battery-bg};
            }
            #battery.warning:not(.charging) {
              color: #${battery-warning};
            }
            #battery.critical:not(.charging) {
              color: #${battery-critical};
            }
            /* ===Tray=== */
            #tray {
              font-family: Material Design Icons, Iosevka Nerd Font Mono;
              font-size: ${stdFontSize}px;
              background-color: #${tray-bg};
              margin: 0px 0px;
              padding: 0px ${stdPadding}px;
            }
            /* ===Clock=== */
            #clock {
              font-family: Material Design Icons, Iosevka Nerd Font Mono;
              font-size: ${stdFontSize}px;
              color: #${clock-fg};
              background-color: #${clock-bg};
              margin: 0px 0px;
              padding: 0px ${stdPadding}px;
            }
            /* ===Backlight=== */
            #backlight {
              font-family: Material Design Icons, Iosevka Nerd Font Mono;
              font-size: ${stdFontSize}px;
              color: #${backlight-fg};
              background-color: #${backlight-bg};
              margin: 0px 0px;
              padding: 0px ${stdPadding}px;
            }
            /* ===Audio=== */
            #pulseaudio {
              font-family: Material Design Icons, Iosevka Nerd Font Mono;
              font-size: ${stdFontSize}px;
              color: #${audio-fg};
              background-color: #${audio-bg};
              margin: 0px 0px;
              padding: 0px ${stdPadding}px;
            }
            /* ===Network=== */
            #network {
              font-family: Material Design Icons, Iosevka Nerd Font Mono;
              font-size: ${stdFontSize}px;
              color: #${network-fg};
              background-color: #${network-bg};
              margin: 0px 0px;
              padding: 0px ${stdPadding}px;
            }
            #network.disconnected,
            #network.disabled {
              color: #${network-disconnected};
            }
            /* ===Power=== */
            #custom-power {
              font-family: Material Design Icons;
              font-size: ${power-font-size}px;
              color: #${power-fg};
              background-color: #${power-bg};
              margin: 0px 0px;
              padding: 0px ${stdPadding}px;
            }
            /* ===CPU=== */
            #cpu {
              font-family: Material Design Icons, Iosevka Nerd Font Mono;
              font-size: ${stdFontSize}px;
              color: #${cpu-fg};
              background-color: #${cpu-bg};
              margin: 0px 0px;
              padding: 0px ${stdPadding}px;
            }
            /* ===Memory=== */
            #memory {
              font-family: Material Design Icons, Iosevka Nerd Font Mono;
              font-size: ${stdFontSize}px;
              color: #${memory-fg};
              background-color: #${memory-bg};
              margin: 0px 0px;
              padding: 0px ${stdPadding}px;
            }
            /* ===MPD=== */
            #mpd {
              font-family: Material Design Icons, Iosevka Nerd Font Mono;
              font-size: ${stdFontSize}px;
              color: #${mpd-fg};
              background-color: #${mpd-bg};
              margin: 0px 0px;
              padding: 0px ${stdPadding}px;
            }
          '';
        settings = [
          {
            layer = "top";
            position = "top";
            output = [
              "eDP-1"
              "DP-2"
            ];
            modules-left = [ "hyprland/workspaces" ];
            modules-right = [
              "battery"
              "tray"
              "cpu"
              "memory"
              "backlight"
              "network"
              "pulseaudio"
              "clock"
            ];
            "custom/launcher" = {
              format = "󱄅";
              tooltip = false;
              on-click = "sleep 0.1 && ${config.nocturne.wayland.menu.exec}";
            };
            # margin-top = 5;
            "hyprland/workspaces" = {
              on-click = "activate";
              format = "{icon}";
              active-only = false;
              all-outputs = true;
              # format-icons = {
              #   "1" = "一";
              #   "2" = "二";
              #   "3" = "三";
              #   "4" = "四";
              #   "5" = "五";
              #   "6" = "六";
              # };
              persistent-workspaces = {
                "*" = [
                  1
                  2
                  3
                  4
                  5
                  6
                ];
              };
            };
            battery = {
              states = {
                warning = 30;
                critical = 15;
              };
              interval = 15;
              format = "{icon} {capacity}%";
              format-charging = "󰂄 {capacity}%";
              format-plugged = "󰂄 {capacity}%";
              format-alt = "{icon} {capacity}%";
              format-icons = [
                "󰂃"
                "󰁺"
                "󰁻"
                "󰁼"
                "󰁽"
                "󰁾"
                "󰁿"
                "󰂀"
                "󰂁"
                "󰂂"
                "󰁹"
              ];
            };
            tray = {
              icon-size = config.nocturne.wayland.waybar.stdIconSize;
              spacing = config.nocturne.wayland.waybar.stdPadding;
            };
            # Center
            clock = {
              interval = 60;
              format = "󰅐 {:%H:%M}";
              tooltip = true;
              tooltip-format = "󰃰 {:%m/%d (%a)}";
              max-length = 25;
            };
            # Right
            backlight = {
              format = "{icon} {percent}%";
              format-icons = [
                "󰋙"
                "󰫃"
                "󰫄"
                "󰫅"
                "󰫆"
                "󰫇"
                "󰫈"
              ];
              on-scroll-up = "${pkgs.brightnessctl}/bin/brightnessctl s +5%";
              on-scroll-down = "${pkgs.brightnessctl}/bin/brightnessctl s 5%-";
            };
            pulseaudio = {
              scroll-step = 5;
              on-click = "sleep 0.1 && ${pkgs.pavucontrol}/bin/pavucontrol";
              tooltip = true;
              tooltip-format = "{volume}%";
              format = "{icon} {volume}%";
              format-muted = "󰝟 MUTE";
              format-icons = {
                default = [
                  "󰕿"
                  "󰖀"
                  "󰕾"
                ];
              };
            };

            network = {
              format = "{ifname}";
              format-wifi = "󰤨";
              format-ethernet = "󰈀";
              format-disconnected = "󰤭";
              tooltip-format = "󰛳 {ifname} via {gwaddr}";
              tooltip-format-wifi = "󰤨 {essid}";
              tooltip-format-ethernet = "󰈀 {ipaddr}/{cidr}";
              tooltip-format-disconnected = "Disconnected";
              max-length = "25";
            };
            "custom/power" = {
              format = "󰐥";
              tooltip = false;
              on-click = "sleep 0.1 && ${config.nocturne.wayland.menu.exec-logout}";
            };
            cpu = {
              format = "󰻠 {usage}%";
            };
            memory = {
              format = "󰍛 {percentage}%";
            };
            mpd = {
              format = "{stateIcon} {title}";
              format-stopped = "󰓛 Stopped";
              format-paused = "{stateIcon} {title}";
              max-length = 15;
              on-click = "${pkgs.mpc-cli}/bin/mpc toggle";
              on-click-middle = "${pkgs.mpc-cli}/bin/mpc stop";
              on-click-right = "${config.nocturne.wayland.terminal.exec-center} ${pkgs.ncmpcpp}/bin/ncmpcpp";
              state-icons = {
                paused = "󰂼";
                playing = "󱉺";
              };
              tooltip = true;
              tooltip-format = "| Queue: {songPosition}/{queueLength}\n| Time: {elapsedTime:%M:%S} - {totalTime:%M:%S}\n| Song: {title}\n| Artist: {artist}\n| Album: {album}";
            };
          }
        ];
      };
    })
    (lib.mkIf (uiStyle == "fancy") {
      programs.waybar = {
        enable = true;
        package = pkgs.waybar;
        style =
          let
            stdMargin = builtins.toString config.nocturne.wayland.waybar.stdMargin;
            stdPadding = builtins.toString config.nocturne.wayland.waybar.stdPadding;
            stdFontSize = builtins.toString config.nocturne.wayland.waybar.stdFontSize;
            workspace-bg = config.nocturne.wayland.waybar.workspace-bg;
            workspace-hover-bg = config.nocturne.wayland.waybar.workspace-hover-bg;
            workspace-fg = config.nocturne.wayland.waybar.workspace-fg;
            workspace-empty = config.nocturne.wayland.waybar.workspace-empty;
            workspace-urgent = config.nocturne.wayland.waybar.workspace-urgent;
            workspace-visible = config.nocturne.wayland.waybar.workspace-visible;
            launcher-font-size = builtins.toString config.nocturne.wayland.waybar.launcher-font-size;
            launcher-fg = config.nocturne.wayland.waybar.launcher-fg;
            launcher-bg = config.nocturne.wayland.waybar.launcher-bg;
            battery-fg = config.nocturne.wayland.waybar.battery-fg;
            battery-bg = config.nocturne.wayland.waybar.battery-bg;
            battery-warning = config.nocturne.wayland.waybar.battery-warning;
            battery-critical = config.nocturne.wayland.waybar.battery-critical;
            clock-fg = config.nocturne.wayland.waybar.clock-fg;
            clock-bg = config.nocturne.wayland.waybar.clock-bg;
            tray-bg = config.nocturne.wayland.waybar.tray-bg;
            backlight-fg = config.nocturne.wayland.waybar.backlight-fg;
            backlight-bg = config.nocturne.wayland.waybar.backlight-bg;
            audio-fg = config.nocturne.wayland.waybar.audio-fg;
            audio-bg = config.nocturne.wayland.waybar.audio-bg;
            network-fg = config.nocturne.wayland.waybar.network-fg;
            network-bg = config.nocturne.wayland.waybar.network-bg;
            network-disconnected = config.nocturne.wayland.waybar.network-disconnected;
            power-font-size = builtins.toString config.nocturne.wayland.waybar.power-font-size;
            power-fg = config.nocturne.wayland.waybar.power-fg;
            power-bg = config.nocturne.wayland.waybar.power-bg;
            cpu-fg = config.nocturne.wayland.waybar.cpu-fg;
            cpu-bg = config.nocturne.wayland.waybar.cpu-bg;
            memory-fg = config.nocturne.wayland.waybar.memory-fg;
            memory-bg = config.nocturne.wayland.waybar.memory-bg;
            mpd-fg = config.nocturne.wayland.waybar.mpd-fg;
            mpd-bg = config.nocturne.wayland.waybar.mpd-bg;
            tooltip-bg = config.nocturne.wayland.waybar.tooltip-bg;
            tooltip-fg = config.nocturne.wayland.waybar.tooltip-fg;
          in
          ''
            * {
              border: none;
              border-radius: 8px;
            }
            tooltip {
              font-family: Material Design Icons, Iosevka Nerd Font Mono, DejaVu Sans;
              box-shadow: none;
              text-shadow: none;
              background: none;
              background-color: #${tooltip-bg};
              font-size: ${stdFontSize}px;
            }
            tooltip * {
              font-family: Material Design Icons, Iosevka Nerd Font Mono, DejaVu Sans;
              box-shadow: none;
              text-shadow: none;
              background: none;
              color: #${tooltip-fg};
              font-size: ${stdFontSize}px;
              padding: 0px 0px;
            }
            window#waybar {
              background: transparent;
              /* background-color: #2e3440; */
              /* color: #d8dee9; */
            }
            /* ===Workspaces=== */
            #workspaces {
              background-color: #${workspace-bg};
              margin: 0px ${stdMargin}px;
              padding: 0px ${stdPadding}px;
            }
            #workspaces button {
              color: #${workspace-fg};
              font-family: Material Design Icons, Iosevka Nerd Font Mono, DejaVu Sans;
              font-size: ${stdFontSize}px;
              margin: 0px;
              padding: 0px ${stdPadding}px;
            }
            #workspaces button:hover {
              box-shadow: none;
              text-shadow: none;
              background: none;
              background-color: #${workspace-hover-bg};
            }
            #workspaces button.empty {
              color: #${workspace-empty};
            }
            #workspaces button.active {
              color: #${workspace-fg};
            }
            #workspaces button.urgent {
              color: #${workspace-urgent};
            }
            #workspaces button.visible {
              color: #${workspace-visible};
            }
            /* ===Launcher=== */
            #custom-launcher {
              font-family: Material Design Icons;
              font-size: ${launcher-font-size}px;
              color: #${launcher-fg};
              background-color: #${launcher-bg};
              margin: 0px ${stdMargin}px;
              padding: 0px ${stdPadding}px;
            }
            /* ===Battery=== */
            #battery {
              font-family: Material Design Icons, Iosevka Nerd Font Mono;
              font-size: ${stdFontSize}px;
              margin: 0px ${stdMargin}px;
              padding: 0px ${stdPadding}px;
              color: #${battery-fg};
              background-color: #${battery-bg};
            }
            #battery.warning:not(.charging) {
              color: #${battery-warning};
            }
            #battery.critical:not(.charging) {
              color: #${battery-critical};
            }
            /* ===Tray=== */
            #tray {
              font-family: Material Design Icons, Iosevka Nerd Font Mono;
              font-size: ${stdFontSize}px;
              background-color: #${tray-bg};
              margin: 0px ${stdMargin}px;
              padding: 0px ${stdPadding}px;
            }
            /* ===Clock=== */
            #clock {
              font-family: Material Design Icons, Iosevka Nerd Font Mono;
              font-size: ${stdFontSize}px;
              color: #${clock-fg};
              background-color: #${clock-bg};
              margin: 0px ${stdMargin}px;
              padding: 0px ${stdPadding}px;
            }
            /* ===Backlight=== */
            #backlight {
              font-family: Material Design Icons, Iosevka Nerd Font Mono;
              font-size: ${stdFontSize}px;
              color: #${backlight-fg};
              background-color: #${backlight-bg};
              margin: 0px ${stdMargin}px;
              padding: 0px ${stdPadding}px;
            }
            /* ===Audio=== */
            #pulseaudio {
              font-family: Material Design Icons, Iosevka Nerd Font Mono;
              font-size: ${stdFontSize}px;
              color: #${audio-fg};
              background-color: #${audio-bg};
              margin: 0px ${stdMargin}px;
              padding: 0px ${stdPadding}px;
            }
            /* ===Network=== */
            #network {
              font-family: Material Design Icons, Iosevka Nerd Font Mono;
              font-size: ${stdFontSize}px;
              color: #${network-fg};
              background-color: #${network-bg};
              margin: 0px ${stdMargin}px;
              padding: 0px ${stdPadding}px;
            }
            #network.disconnected,
            #network.disabled {
              color: #${network-disconnected};
            }
            /* ===Power=== */
            #custom-power {
              font-family: Material Design Icons;
              font-size: ${power-font-size}px;
              color: #${power-fg};
              background-color: #${power-bg};
              margin: 0px ${stdMargin}px;
              padding: 0px ${stdPadding}px;
            }
            /* ===CPU=== */
            #cpu {
              font-family: Material Design Icons, Iosevka Nerd Font Mono;
              font-size: ${stdFontSize}px;
              color: #${cpu-fg};
              background-color: #${cpu-bg};
              margin: 0px ${stdMargin}px;
              padding: 0px ${stdPadding}px;
            }
            /* ===Memory=== */
            #memory {
              font-family: Material Design Icons, Iosevka Nerd Font Mono;
              font-size: ${stdFontSize}px;
              color: #${memory-fg};
              background-color: #${memory-bg};
              margin: 0px ${stdMargin}px;
              padding: 0px ${stdPadding}px;
            }
            /* ===MPD=== */
            #mpd {
              font-family: Material Design Icons, Iosevka Nerd Font Mono;
              font-size: ${stdFontSize}px;
              color: #${mpd-fg};
              background-color: #${mpd-bg};
              margin: 0px ${stdMargin}px;
              padding: 0px ${stdPadding}px;
            }
          '';

        settings = [
          {
            layer = "top";
            position = "top";
            output = [
              "eDP-1"
              "DP-2"
            ];
            modules-left = [
              "custom/launcher"
              "battery"
              "backlight"
              "mpd"
              "tray"
            ];
            modules-center = [ "hyprland/workspaces" ];
            modules-right = [
              "cpu"
              "memory"
              "pulseaudio"
              "network"
              "clock"
              "custom/power"
            ];
            "custom/launcher" = {
              format = "󱄅";
              tooltip = false;
              on-click = "sleep 0.1 && ${config.nocturne.wayland.menu.exec}";
            };
            margin-top = 5;
            "hyprland/workspaces" = {
              on-click = "activate";
              format = "{icon}";
              active-only = false;
              all-outputs = true;
              format-icons = {
                "1" = "一";
                "2" = "二";
                "3" = "三";
                "4" = "四";
                "5" = "五";
                "6" = "六";
              };
              persistent-workspaces = {
                "*" = [
                  1
                  2
                  3
                  4
                  5
                  6
                ];
              };
            };
            battery = {
              states = {
                warning = 30;
                critical = 15;
              };
              interval = 15;
              format = "{icon} {capacity}%";
              format-charging = "󰂄 {capacity}%";
              format-plugged = "󰂄 {capacity}%";
              format-alt = "{icon} {capacity}%";
              format-icons = [
                "󰂃"
                "󰁺"
                "󰁻"
                "󰁼"
                "󰁽"
                "󰁾"
                "󰁿"
                "󰂀"
                "󰂁"
                "󰂂"
                "󰁹"
              ];
            };
            tray = {
              icon-size = config.nocturne.wayland.waybar.stdIconSize;
              spacing = config.nocturne.wayland.waybar.stdPadding;
            };
            # Center
            clock = {
              interval = 60;
              format = "󰅐 {:%H:%M}";
              tooltip = true;
              tooltip-format = "󰃰 {:%m/%d (%a)}";
              max-length = 25;
            };
            # Right
            backlight = {
              format = "{icon} {percent}%";
              format-icons = [
                "󰋙"
                "󰫃"
                "󰫄"
                "󰫅"
                "󰫆"
                "󰫇"
                "󰫈"
              ];
              on-scroll-up = "${pkgs.brightnessctl}/bin/brightnessctl s +5%";
              on-scroll-down = "${pkgs.brightnessctl}/bin/brightnessctl s 5%-";
            };
            pulseaudio = {
              scroll-step = 5;
              on-click = "sleep 0.1 && ${pkgs.pavucontrol}/bin/pavucontrol";
              tooltip = true;
              tooltip-format = "{volume}%";
              format = "{icon} {volume}%";
              format-muted = "󰝟 MUTE";
              format-icons = {
                default = [
                  "󰕿"
                  "󰖀"
                  "󰕾"
                ];
              };
            };
            network = {
              format = "{ifname}";
              format-wifi = "󰤨";
              format-ethernet = "󰈀";
              format-disconnected = "󰤭";
              tooltip-format = "󰛳 {ifname} via {gwaddr}";
              tooltip-format-wifi = "󰤨 {essid}";
              tooltip-format-ethernet = "󰈀 {ipaddr}/{cidr}";
              tooltip-format-disconnected = "Disconnected";
              max-length = "25";
            };
            "custom/power" = {
              format = "󰐥";
              tooltip = false;
              on-click = "sleep 0.1 && ${config.nocturne.wayland.menu.exec-logout}";
            };
            cpu = {
              format = "󰻠 {usage}%";
            };
            memory = {
              format = "󰍛 {percentage}%";
            };
            mpd = {
              format = "{stateIcon} {title}";
              format-stopped = "󰓛 Stopped";
              format-paused = "{stateIcon} {title}";
              max-length = 15;
              on-click = "${pkgs.mpc-cli}/bin/mpc toggle";
              on-click-middle = "${pkgs.mpc-cli}/bin/mpc stop";
              on-click-right = "${config.nocturne.wayland.terminal.exec-center} ${pkgs.ncmpcpp}/bin/ncmpcpp";
              state-icons = {
                paused = "󰂼";
                playing = "󱉺";
              };
              tooltip = true;
              tooltip-format = "| Queue: {songPosition}/{queueLength}\n| Time: {elapsedTime:%M:%S} - {totalTime:%M:%S}\n| Song: {title}\n| Artist: {artist}\n| Album: {album}";
            };
          }
        ];
      };
    })
  ];
}
