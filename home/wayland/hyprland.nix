{
  config,
  pkgs,
  lib,
  mylib,
  isLaptop,
  ...
}:
let
  # ===Application Launcher(s)===
  menu-drun = config.nocturne.wayland.menu.exec;
  menu-run = config.nocturne.wayland.menu.exec-run;
  # menu-window = config.nocturne.wayland.menu.window;

  # ===Notification Scripts===
  volup = config.nocturne.wayland.notification.exec-volup;
  voldown = config.nocturne.wayland.notification.exec-voldown;
  volmute = config.nocturne.wayland.notification.exec-volmute;
  brightup = config.nocturne.wayland.notification.exec-brightup;
  brightdown = config.nocturne.wayland.notification.exec-brightdown;

  # ===Bar===
  bar-on = config.nocturne.wayland.bar.exec-on;
  bar-off = config.nocturne.wayland.bar.exec-off;
  bar-start = config.nocturne.wayland.bar.exec-start;
  bar-toggle = config.nocturne.wayland.bar.exec-toggle;

  # ===Editor===
  ed-cfg = config.nocturne.wayland.editor;

  # ===Terminal===
  term-cfg = config.nocturne.wayland.terminal;

  # ===Border Colors===
  col_active_border1 = config.nocturne.wayland.hyprland.col-active-border1;
  col_active_border2 = config.nocturne.wayland.hyprland.col-active-border2;
  col_background = config.nocturne.wayland.hyprland.col-background;
  col_inactive_border = config.nocturne.wayland.hyprland.col-inactive-border;

  # ===Rounding===
  rounding = 10;

  # ===Gaps===
  gaps_out = config.nocturne.wayland.decoration.stdgaps;
  gaps_in = gaps_out / 2;

  # ===Plugins===
  inherit (config.nocturne.wayland.hyprland) plugins;

  # ===Monitor Configurations===
  monitorConfig = mylib.hyprlandDefaultProfile "undocked" config.nocturne.wayland.monitor-profiles;

  # ===Checked Hyprdock===
  checkedHyprdock = pkgs.writeShellApplication {
    name = "checked-hyprdock";
    runtimeInputs = with pkgs; [
      hyprland
      dfh
    ];
    text = ''
      if hyprdock -a "$1" | monchk --; then
          hyprdock "$1"
      fi
    '';
  };

  # ===Motion Key DWIM===
  hjklDwim = pkgs.writeShellApplication {
    name = "hjkl-dwim";
    runtimeInputs = with pkgs; [
      hyprland
      jq
    ];
    text = ''
      FLOATING="$(hyprctl activewindow -j | jq ".floating" | sed 's/"//g')"
      LAYOUT="$(hyprctl getoption "general:layout" -j | jq ".str" | sed 's/"//g')"
      if [ -z "$1" ]; then
        echo "No Arguments Given" 1>&2
        exit 1
      fi
      function floating() {
        case "$1" in
          h) hyprctl dispatch "moveactive -25 0" ;;
          j) hyprctl dispatch "moveactive 0 25" ;;
          k) hyprctl dispatch "moveactive 0 -25" ;;
          l) hyprctl dispatch "moveactive 25 0" ;;
          H) hyprctl dispatch "resizeactive -25 0" ;;
          J) hyprctl dispatch "resizeactive 0 25" ;;
          K) hyprctl dispatch "resizeactive 0 -25" ;;
          L) hyprctl dispatch "resizeactive 25 0" ;;
          *) echo "Expected one of 'h', 'j', 'k', or 'l', got $1" 1>&2 && exit 1;;
        esac
      }
      function master() {
        case "$1" in
          h) hyprctl dispatch "layoutmsg swapprev" ;;
          j) hyprctl dispatch "layoutmsg rollnext" ;;
          k) hyprctl dispatch "layoutmsg rollprev" ;;
          l) hyprctl dispatch "layoutmsg swapnext" ;;
          H) hyprctl dispatch "layoutmsg mfact -0.01" ;;
          J) hyprctl dispatch "layoutmsg cyclenext" ;;
          K) hyprctl dispatch "layoutmsg cycleprev" ;;
          L) hyprctl dispatch "layoutmsg mfact 0.01" ;;
          *) echo "Expected one of 'h', 'j', 'k', or 'l', got $1" 1>&2 && exit 1;;
        esac
      }
      function dwindle() {
        case "$1" in
          h) hyprctl dispatch "movefocus l";;
          j) hyprctl dispatch "movefocus d";;
          k) hyprctl dispatch "movefocus u";;
          l) hyprctl dispatch "movefocus r";;
          H) hyprctl dispatch "swapwindow l";;
          J) hyprctl dispatch "swapwindow d";;
          K) hyprctl dispatch "swapwindow u";;
          L) hyprctl dispatch "swapwindow r";;
          t) hyprctl dispatch "layoutmsg swapsplit" ;;
          *) echo "Expected one of 'h', 'j', 'k', or 'l', got $1" 1>&2 && exit 1;;
        esac
      }
      if [ "$FLOATING" = "true" ]; then
        floating "$1"
      elif [ "$LAYOUT" = "dwindle" ]; then
        dwindle "$1"
      else
        master "$1"
      fi
    '';
  };

  # ===Toggle Animations===
  toggleAnimations = pkgs.writeShellApplication {
    name = "toggle-animations";
    runtimeInputs = with pkgs; [
      hyprland
      jq
    ];
    text = ''
      ANIMATIONS="$(hyprctl -j getoption animations:enabled | jq ".int" | sed 's/"//g')"
      if [ "$ANIMATIONS" = "1" ]; then
        hyprctl keyword animations:enabled false
      else
        hyprctl keyword animations:enabled true
      fi
    '';
  };
  # ===Floating Script===
  centerAllFloating = pkgs.writeShellApplication {
    name = "center-all-float";
    runtimeInputs = with pkgs; [
      hyprland
      jq
    ];
    text = ''
      CURRENT_ACTIVE_WORKSPACE="$(hyprctl -j activeworkspace | jq ".id")"
      WINDOWS_IN_WORKSPACE="$(hyprctl clients -j | jq ".[] | select(.workspace.id==$CURRENT_ACTIVE_WORKSPACE) | select(.floating==true) | .address" | sed 's/"//g')"
      ORIGINAL_WINDOW="$(hyprctl activewindow -j | jq ".address" | sed 's/"//g')"
      for i in $WINDOWS_IN_WORKSPACE; do
        hyprctl dispatch focuswindow address:"$i"
        hyprctl dispatch centerwindow 1
      done
      hyprctl dispatch focuswindow address:"$ORIGINAL_WINDOW"
    '';
  };

  focusMode = pkgs.writeShellApplication {
    name = "focus-mode";
    runtimeInputs = [
      pkgs.hyprland
      pkgs.killall
    ];
    text =
      let
        menu = config.nocturne.wayland.menu;
      in
      ''
        MODE=$(echo -e "󰿄 Enter\n󰿅 Exit" | ${menu.exec-dmenu} ${menu.promptSwitch} "Focus Mode" | awk '{print tolower($2)}')
        case "$MODE" in
          enter)
            hyprctl --batch 'keyword general:gaps_in 0 ; keyword general:gaps_out 0 ; keyword decoration:rounding 0'
            ${bar-off} ;;
          exit)
            hyprctl --batch 'keyword general:gaps_in ${builtins.toString gaps_in} ; keyword general:gaps_out ${builtins.toString gaps_out} ; keyword decoration:rounding ${builtins.toString rounding}'
            ${bar-off} || true
            ${bar-on} ;;
        esac
      '';
  };
in
{
  config = lib.mkMerge [
    {
      # ===Packages Needed===
      home.packages = [
        hjklDwim
        centerAllFloating
        checkedHyprdock
        focusMode
        pkgs.dfh
        pkgs.killall
        # Audio Control
        pkgs.pavucontrol
        pkgs.swww
        pkgs.wl-clipboard
      ];

      # ===Hyprland Autostart===
      nocturne.wayland.compositor.profileExtra = ''
        [[ $(tty) == /dev/tty1 ]] && exec Hyprland && exit 0
      '';

      # ===Actual Hyprland Config===
      wayland.windowManager.hyprland = {
        enable = true;
        # package = inputs.hyprland.packages.${pkgs.system}.hyprland;
        plugins = with pkgs.hyprlandPlugins; [ ] ++ lib.optionals plugins.hyprbars.enable [ hyprbars ];
        settings = {
          "$terminal" = "${term-cfg.exec}";
          "$editor" = "${ed-cfg.exec}";
          "$fileManager" = "dolphin";
          "$menu-drun" = menu-drun;
          "$menu-run" = menu-run;
          # "$menu-window" = menu-window;
          "$MOD" = "SUPER";

          env = [ "XCURSOR_SIZE,24" ];
          monitor = monitorConfig;
          exec-once =
            [
              "${bar-start}"
              "${pkgs.swww}/bin/swww init"
              "${config.nocturne.wayland.notification.exec-start}"
              "${pkgs.networkmanagerapplet}/bin/nm-applet"
              "[workspace 2 silent] $terminal"
              "[workspace 3 silent] $editor"
            ]
            ++ lib.optionals (config.nocturne.wayland.idleManager.name != null) [
              "${config.nocturne.wayland.idleManager.exec}"
            ]
            ++ lib.optionals (config.nocturne.graphical.firefox.enable) [ "[workspace 4 silent] firefox" ]
            ++ lib.optionals (config.nocturne.graphical.mullvadBrowser.enable) [
              "[workspace 4 silent] mullvad-browser"
            ]
            ++ lib.optionals (config.nocturne.graphical.mullvad-vpn.enable) [
              "[workspace 4 silent] mullvad-vpn"
            ]
            ++ lib.optionals (config.nocturne.graphical.keepassxc.enable) [ "[workspace 5 silent] keepassxc" ]
            ++ lib.optionals (term-cfg.exec-start != null) [ "${term-cfg.exec-start}" ];
          general = {
            inherit gaps_in gaps_out;
            "col.active_border" = "rgba(${col_active_border1}) rgba(${col_active_border2}) 45deg";
            "col.inactive_border" = "rgba(${col_inactive_border})";
            border_size = 3;
            layout = "master";
            allow_tearing = false;
          };
          input = {
            kb_layout = "us";
            follow_mouse = 2;
            touchpad = {
              natural_scroll = "no";
            };
            sensitivity = 0;
          };
          decoration = {
            inherit rounding;
            blur = {
              enabled = true;
              size = 5;
              passes = 2;
            };
            shadow = {
              enabled = false;
            };
          };
          animations = {
            enabled = "yes";
            bezier = "myBezier, 0.05, 0.9, 0.1, 1.05";
            animation = [
              "windows, 1, 7, myBezier"
              "windowsOut, 1, 7, default, popin 80%"
              "border, 1, 10, default"
              "borderangle, 1, 8, default"
              "fade, 1, 7, default"
              "workspaces, 1, 6, default"
            ];
          };
          dwindle = {
            pseudotile = "yes";
            preserve_split = "yes";
          };
          master = {
            new_status = "master";
            orientation = "left";
            mfact = "0.5";
          };
          gestures.workspace_swipe = "off";
          misc = {
            # Anime lady hehe
            force_default_wallpaper = -1;
            disable_hyprland_logo = true;
            background_color = "rgba(${col_background})";
            enable_swallow = true;
            swallow_regex = [ "^(Alacritty)$" ];
          };
          windowrulev2 = [
            "suppressevent maximize, class:.*"
            "size 80% 80%, class:.*" # have windows be a reasonable size
            "center 1, class:.*" # have windows centered by default

            # Title Bar
            "plugin:hyprbars:nobar, floating:0" # no bar on tiled windows
            "plugin:hyprbars:nobar, initialTitle:^(vesktop)$" # no bar vesktop loading window

            # Center
            "float, class:^(center)$"
            "size 80% 85%, class:^(center)$"
            "center, class:^(center)$"

            # Discord/Vesktop
            "workspace 1, class:^(vesktop)$, title:^(.*)$"

            # Pavucontrol
            # "float, class:^(pavucontrol)$, title:^(Volume Control)$"
            # "size 80% 85%, class:^(pavucontrol)$, title:^(Volume Control)$"
            # "center, class:^(pavucontrol)$, title:^(Volume Control)$"

            # Network Manager Applet
            "float, class:^(nm-connection-editor)$, title:^(.*)$"
            "size 80% 85%, class:^(nm-connection-editor)$, title:^(.*)$"
            "center, class:^(nm-connection-editor)$, title:^(.*)$"

            # Signal
            "workspace 1, class:^(Signal)$, title:^(.*)$"
          ];
          bind =
            [
              # General Keybindings
              "$MOD_SHIFT, Return, exec, $terminal"
              "$MOD, Return, exec, $terminal"
              "$MOD, E, exec, $editor"
              "$MOD, R, exec, $menu-run"
              "$MOD, P, exec, $menu-drun"
              "$MOD_SHIFT, A, exec, ${lib.getExe toggleAnimations}"
              # "$MOD, code:61, exec, $menu-window"
              "$MOD, B, exec, ${bar-toggle}"
              "$MOD, D, killactive, "
              "$MOD_CTRL, Q, exit,"
              ## Logout (semi-colon)
              "$MOD_CTRL, code:47, exec, ${config.nocturne.wayland.menu.exec-logout}"
              "$MOD, code:47, exec, ${config.nocturne.wayland.menu.exec-logout}"
              ## Focus Mode
              "$MOD_SHIFT, F, exec, ${lib.getExe focusMode}"

              # Window Manipulation
              "$MOD, Return, layoutmsg, swapwithmaster"
              "$MOD, TAB, cyclenext"
              "$MOD, TAB, bringactivetotop"
              "$MOD, O, cyclenext"
              "$MOD, O, bringactivetotop"
              "$MOD, N, layoutmsg, orientationcycle left top center"
              "$MOD, F, fullscreen"
              "$MOD, M, fullscreen, 1"
              "$MOD, W, togglefloating"
              "$MOD, C, exec, hyprctl dispatch centerwindow 1"
              "$MOD_SHIFT, C, exec, ${lib.getExe centerAllFloating}"

              # Workspace Manipulation
              "$MOD, 1, workspace, 1"
              "$MOD, 2, workspace, 2"
              "$MOD, 3, workspace, 3"
              "$MOD, 4, workspace, 4"
              "$MOD, 5, workspace, 5"
              "$MOD, 6, workspace, 6"
              "$MOD SHIFT, 1, movetoworkspace, 1"
              "$MOD SHIFT, 2, movetoworkspace, 2"
              "$MOD SHIFT, 3, movetoworkspace, 3"
              "$MOD SHIFT, 4, movetoworkspace, 4"
              "$MOD SHIFT, 5, movetoworkspace, 5"
              "$MOD SHIFT, 6, movetoworkspace, 6"

              # Screenshot Commands (if enabled)
            ]
            ++ lib.optionals (config.nocturne.wayland.screenshot.name != null) [
              "$MOD, S, exec, ${lib.getExe config.nocturne.wayland.screenshot.scrn}"
              "$MOD_SHIFT, S, exec, ${lib.getExe config.nocturne.wayland.screenshot.scrn-region}"
            ];

          binde = [
            # Window Management
            "$MOD, H, exec, ${lib.getExe hjklDwim} h"
            "$MOD, J, exec, ${lib.getExe hjklDwim} j"
            "$MOD, K, exec, ${lib.getExe hjklDwim} k"
            "$MOD, L, exec, ${lib.getExe hjklDwim} l"
            "$MOD_SHIFT, H, exec, ${lib.getExe hjklDwim} H"
            "$MOD_SHIFT, J, exec, ${lib.getExe hjklDwim} J"
            "$MOD_SHIFT, K, exec, ${lib.getExe hjklDwim} K"
            "$MOD_SHIFT, L, exec, ${lib.getExe hjklDwim} L"
            "$MOD, T, exec, ${lib.getExe hjklDwim} t"

            # Volume and Brightness
            ", XF86AudioLowerVolume, exec, ${voldown}"
            ", XF86AudioRaiseVolume, exec, ${volup}"
            ", XF86AudioMute, exec, ${volmute}"
            ", XF86MonBrightnessUp, exec, ${brightup}"
            ", XF86MonBrightnessDown, exec, ${brightdown}"
          ];
          bindm = [
            # Mouse Keybindings
            "$MOD, mouse:272, movewindow"
            "$MOD, mouse:273, resizewindow"
          ];
          bindl =
            [ ]
            # Dock when closing Laptop lid
            ++ lib.optionals (isLaptop == true) [
              ",switch:on:Lid Switch,exec,${lib.getExe checkedHyprdock} docked"
              ",switch:off:Lid Switch,exec,${pkgs.dfh}/bin/hyprdock undocked"
            ];
          plugin = lib.mkMerge [
            (lib.mkIf plugins.hyprbars.enable {
              hyprbars =
                let
                  coltext = config.nocturne.graphical.alacritty.fg + "ff";
                  # bar_color = config.nocturne.graphical.alacritty.bg + "d9";
                  bar_color = config.nocturne.themes.colors.base01 + "ff";
                  close_color = config.nocturne.themes.colors.base08;
                  fullscreen_color = config.nocturne.themes.colors.base0A;
                  floating_color = config.nocturne.themes.colors.base0B;
                in
                {
                  bar_height = 20;
                  bar_color = "rgba(${bar_color})";
                  bar_text_font = "JetBrainsMono Nerd Font Bold";
                  bar_text_size = 11;
                  bar_precedence_over_border = true;
                  "col.text" = "rgba(${coltext})";
                  hyprbars-button = [
                    "rgb(${close_color}), 15, , hyprctl dispatch killactive"
                    "rgb(${fullscreen_color}), 15, , hyprctl dispatch fullscreen 1"
                    "rgb(${floating_color}), 15, , hyprctl dispatch togglefloating"
                  ];
                };
            })
          ];
        };
        # Devices
        extraConfig = ''
          device {
              name = at-translated-set-2-keyboard
              kb_options = ctrl:nocaps
          }
        '';
      };
    }
  ];
}
