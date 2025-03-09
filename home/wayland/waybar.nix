{
  config,
  pkgs,
  lib,
  ...
}:
let
  wcfg = config.nocturne.wayland;
  inherit (wcfg) bar waybar;
in
{
  config = lib.mkIf (bar.name == "waybar") {
    home.packages = with pkgs; [
      material-design-icons
      nerd-fonts.jetbrains-mono
      nerd-fonts.iosevka
      nerd-fonts.iosevka-term
      nerd-fonts.iosevka-term-slab
    ];
    programs.waybar = {
      enable = true;
      package = pkgs.waybar;
      style =
        let
          stdMargin = builtins.toString waybar.stdMargin;
          stdPadding = builtins.toString waybar.stdPadding;
          stdFontSize = builtins.toString waybar.stdFontSize;
          launcher-font-size = builtins.toString waybar.launcher-font-size;
          power-font-size = builtins.toString waybar.power-font-size;
          inherit (waybar)
            workspace-bg
            workspace-hover-bg
            workspace-fg
            workspace-empty
            workspace-empty-bg
            workspace-urgent
            workspace-urgent-bg
            workspace-active
            workspace-active-bg
            workspace-visible
            workspace-visible-bg
            launcher-fg
            launcher-bg
            battery-fg
            battery-bg
            battery-warning
            battery-warning-bg
            battery-critical
            battery-critical-bg
            clock-fg
            clock-bg
            tray-bg
            backlight-fg
            backlight-bg
            audio-fg
            audio-bg
            network-fg
            network-bg
            network-disconnected
            power-fg
            power-bg
            cpu-fg
            cpu-bg
            memory-fg
            memory-bg
            mpd-fg
            mpd-bg
            tooltip-bg
            tooltip-fg
            ;
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
            background-color: #${workspace-empty-bg};
          }
          #workspaces button.empty:hover {
            box-shadow: none;
            text-shadow: none;
            background: none;
            background-color: #${workspace-hover-bg};
          }
          #workspaces button.active {
            color: #${workspace-active};
            background-color: #${workspace-active-bg};
          }
          #workspaces button.active:hover {
            box-shadow: none;
            text-shadow: none;
            background: none;
            background-color: #${workspace-hover-bg};
          }
          #workspaces button.urgent {
            color: #${workspace-urgent};
            background-color: #${workspace-urgent-bg};
          }
          #workspaces button.urgent:hover {
            box-shadow: none;
            text-shadow: none;
            background: none;
            background-color: #${workspace-hover-bg};
          }
          #workspaces button.visible {
            color: #${workspace-visible};
            background-color: #${workspace-visible-bg};
          }
          #workspaces button.visible:hover {
            box-shadow: none;
            text-shadow: none;
            background: none;
            background-color: #${workspace-hover-bg};
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
            background-color: #${battery-warning-bg};
          }
          #battery.critical:not(.charging) {
            color: #${battery-critical};
            background-color: #${battery-critical-bg};
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
            on-click = "sleep 0.1 && ${wcfg.menu.exec}";
            on-click-right = "sleep 0.1 && ${wcfg.menu.exec-run}";
          };
          margin-top = 5;
          "hyprland/workspaces" = {
            on-click = "activate";
            format = "{icon}";
            active-only = false;
            all-outputs = true;
            format-icons = {
              # "1" = "一";
              # "2" = "二";
              # "3" = "三";
              # "4" = "四";
              # "5" = "五";
              # "6" = "六";
              "1" = "";
              "2" = "";
              "3" = "";
              "4" = "";
              "5" = "";
              "6" = "";
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
            icon-size = waybar.stdIconSize;
            spacing = waybar.stdPadding;
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
            on-click = "sleep 0.1 && ${wcfg.menu.exec-logout}";
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
            on-click-right = "${wcfg.terminal.exec-center} ${pkgs.ncmpcpp}/bin/ncmpcpp";
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
    nocturne.wayland.bar =
      let
        waybar-pkg = config.programs.waybar.package;
        on = pkgs.writeShellApplication {
          name = "waybar-on";
          runtimeInputs = with pkgs; [
            util-linux
            waybar-pkg
          ];
          text = ''
            setsid -f waybar >/dev/null 2>&1
          '';
        };
        off = pkgs.writeShellApplication {
          name = "waybar-off";
          runtimeInputs = with pkgs; [
            killall
            waybar-pkg
          ];
          text = ''
            killall '.waybar-wrapped'
          '';
        };
        toggle = pkgs.writeShellApplication {
          name = "waybar-toggle";
          runtimeInputs = [
            on
            off
          ];
          text = ''
            waybar-off || waybar-on
          '';
        };
      in
      {
        exec-on = "${lib.getExe on}";
        exec-off = "${lib.getExe off}";
        exec-start = "${lib.getExe on}";
        exec-toggle = "${lib.getExe toggle}";
      };
  };
}
