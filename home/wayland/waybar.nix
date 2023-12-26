{ pkgs, ... }: {
  config = {
    home.packages = [ pkgs.material-design-icons ];
    programs.waybar = {
      enable = true;
      package = pkgs.waybar;
      style = let
        stdMargin = builtins.toString 4;
        stdPadding = builtins.toString 6;
        stdLaunchFontSize = builtins.toString 24;
        stdFontSize = builtins.toString 16;
      in ''
        * {
          border: none;
          border-radius: 5px;
        }
        window#waybar {
          background: transparent;
          /* background-color: #2e3440; */
          color: #d8dee9;
        }
        /* ===Workspaces=== */
        #workspaces {
          background-color: #2e3440;
          margin: 0px ${stdMargin}px;
          padding: 0px ${stdPadding}px;
        }
        #workspaces button {
          font-family: Material Design Icons;
          font-size: ${stdFontSize}px;
          margin: 0px;
          padding: 0px ${stdPadding}px;
        }
        #workspaces button.empty {
          color: #4c566a;
        }
        #workspaces button.urgent {
          color: #d08770;
        }
        #workspaces button.visible {
          color: #ebcb8b;
        }
        /* ===Launcher=== */
        #custom-launcher {
          font-family: Material Design Icons;
          font-size: 24px;
          color: #2e3440;
          background-color: #5e81ac;
          margin: 0px ${stdMargin}px;
          padding: 0px ${stdPadding}px;
        }
        /* ===Battery=== */
        #battery {
          font-family: Material Design Icons, Iosevka Nerd Font Mono;
          font-size: ${stdFontSize}px;
          margin: 0px ${stdMargin}px;
          padding: 0px ${stdPadding}px;
          color: #8fbcbb;
          background-color: #2e3440;
        }
        #battery.warning {
          color: #d08770;
        }
        #battery.critical:not(.charging) {
          color: #bf616a;
        }
        /* ===Clock=== */
        #clock {
          font-family: Material Design Icons;
          font-size: ${stdFontSize}px;
          color: #d8dee9;
          background-color: #2e3440;
          margin: 0px ${stdMargin}px;
          padding: 0px ${stdPadding}px;
        }
        /* ===Power=== */
        #custom-power {
          font-family: Material Design Icons;
          font-size: 24px;
          color: #2e3440;
          background-color: #bf616a;
          margin: 0px ${stdMargin}px;
          padding: 0px ${stdPadding}px;
        }
      '';

      settings = [{
        layer = "top";
        position = "top";
        output = [
          "eDP-1"
        ];
        modules-left = [ "custom/launcher" "hyprland/workspaces" "battery" ];
        modules-center = [ "clock" ];
        modules-right = [ "custom/power" ];
        "custom/launcher" = {
          format = "󱄅";
          tooltip = false;
          on-click = "sleep 0.1 && rofi -show drun";
        };
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
            "*" = 6;
          };
        };
        battery = {
          states = {
            warning = 30;
            critical = 15;
          };
          format = "{icon} {capacity}%";
          format-charging = "󰂄 {capacity}%";
          format-plugged = "󰂄 {capacity}%";
          format-alt = "{icon} {capacity}%";
          format-icons = ["󰂃" "󰁺" "󰁻" "󰁼" "󰁽" "󰁾" "󰁿" "󰂀" "󰂁" "󰂂" "󰁹"];
        };
        # Center
        clock = {
          interval = 60;
          format = "󰅐 {:%H:%M}";
          max-length = 25;
        };
        # Right
        "custom/power" = {
          format = "󰐥";
          tooltip = false;
          on-click = "sleep 0.1 && ${pkgs.wlogout}/bin/wlogout";
        };
      }];
    };
  };
}
