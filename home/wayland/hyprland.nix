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

  # ===Editor===
  ed-cfg = config.nocturne.wayland.editor;

  # ===Terminal===
  term-cfg = config.nocturne.wayland.terminal;

  # ===Border Colors===
  col_active_border1 = config.nocturne.wayland.hyprland.col-active-border1;
  col_active_border2 = config.nocturne.wayland.hyprland.col-active-border2;
  col_background = config.nocturne.wayland.hyprland.col-background;
  col_inactive_border = config.nocturne.wayland.hyprland.col-inactive-border;

  # ===Plugins===
  inherit (config.nocturne.wayland.hyprland) plugins;

  # ===Monitor Configurations===
  monitors = config.nocturne.wayland.monitors;
  initialMonitorConfig = mylib.removeEmpty (
    [ ]
    ++ mylib.monitorsToHyprlandConfigNonDisable monitors "undocked"
    ++ mylib.monitorsToHyprlandConfigNonDisable monitors "docked"
  );
in
{
  config = lib.mkMerge [
    {
      # ===Packages Needed===
      home.packages = [
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
        plugins = with pkgs.hyprlandPlugins; [ ] ++ lib.optionals plugins.hyprbars [ hyprbars ];
        settings = {
          "$terminal" = "${term-cfg.exec}";
          "$editor" = "${ed-cfg.exec}";
          "$fileManager" = "dolphin";
          "$menu-drun" = menu-drun;
          "$menu-run" = menu-run;
          # "$menu-window" = menu-window;
          "$MOD" = "SUPER";

          env = [ "XCURSOR_SIZE,24" ];
          monitor = initialMonitorConfig ++ [ ",preferred,auto,auto" ];
          exec-once =
            [
              "waybar"
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
            ++ lib.optionals (term-cfg.exec-start != null) [ "${term-cfg.exec-start}" ];
          general = {
            gaps_in = 3;
            gaps_out = 5;
            border_size = 2;
            "col.active_border" = "rgba(${col_active_border1}) rgba(${col_active_border2}) 45deg";
            "col.inactive_border" = "rgba(${col_inactive_border})";
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
            rounding = 10;
            blur = {
              enabled = true;
              size = 5;
              passes = 2;
            };
            drop_shadow = "no";
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
            orientation = "center";
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
              # "$MOD, code:61, exec, $menu-window"
              "$MOD, B, exec, killall '.waybar-wrapped' || waybar"
              "$MOD_SHIFT, C, killactive, "
              "$MOD, C, killactive, "
              "$MOD_SHIFT, Q, exit,"
              ## Logout (semi-colon)
              "$MOD_CTRL, code:47, exec, ${config.nocturne.wayland.menu.exec-logout}"
              "$MOD, code:47, exec, ${config.nocturne.wayland.menu.exec-logout}"

              # Window Manipulation
              "$MOD, H, layoutmsg, swapprev"
              "$MOD, L, layoutmsg, swapnext"
              "$MOD, K, layoutmsg, cycleprev"
              "$MOD, J, layoutmsg, cyclenext"
              "$MOD, Return, layoutmsg, swapwithmaster"
              "$MOD, O, layoutmsg, orientationcycle top left center"
              "$MOD, F, fullscreen"
              "$MOD, M, fullscreen, 1"
              "$MOD, W, togglefloating"
              "$MOD, TAB, cyclenext"
              "$MOD, TAB, bringactivetotop"

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
              ",switch:on:Lid Switch,exec,${pkgs.dfh}/bin/hyprdock docked"
              ",switch:off:Lid Switch,exec,${pkgs.dfh}/bin/hyprdock -exclude-disable undocked"
            ];
          plugin = lib.mkMerge [
            (lib.mkIf plugins.hyprbars {
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
                  bar_height = 25;
                  bar_color = "rgba(${bar_color})";
                  bar_text_font = "JetBrainsMono Nerd Font Bold";
                  bar_text_size = 11;
                  bar_precedence_over_border = true;
                  "col.text" = "rgba(${coltext})";
                  hyprbars-button = [
                    "rgb(${close_color}), 20, 󰅖, hyprctl dispatch killactive"
                    "rgb(${fullscreen_color}), 20, 󰊓, hyprctl dispatch fullscreen 2"
                    "rgb(${floating_color}), 20, 󰍴, hyprctl dispatch togglefloating"
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
    (lib.mkIf config.xdg.enable { xdg.configFile."dfh/monitors.json".text = builtins.toJSON monitors; })
  ];
}
