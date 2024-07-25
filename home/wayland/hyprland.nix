{
  config,
  pkgs,
  lib,
  isLaptop,
  ...
}:
let
  # ===Application Launcher(s)===
  menu-drun = config.nocturne.wayland.menu.drun;
  menu-run = config.nocturne.wayland.menu.run;
  menu-window = config.nocturne.wayland.menu.window;

  # ===Editor===
  ed-cfg = config.nocturne.wayland.editor;

  # ===Terminal===
  term-cfg = config.nocturne.wayland.terminal;

  # ===Border Colors===
  col_active_border1 = config.nocturne.wayland.hyprland.col-active-border1;
  col_active_border2 = config.nocturne.wayland.hyprland.col-active-border2;
  col_background = config.nocturne.wayland.hyprland.col-background;
  col_inactive_border = config.nocturne.wayland.hyprland.col-inactive-border;

  # ===Monitor Configurations===
  monitors = map (
    m:
    let
      resolution = "${toString m.width}x${toString m.height}@${toString m.refreshRate}";
      position = "${toString m.x}x${toString m.y}";
    in
    "${m.name},${resolution},${position},${toString m.scale}"
  ) config.nocturne.wayland.monitors;

  # ===Dock Station Monitor Configurations===
  docked-monitors = map (
    m:
    let
      resolution = "${toString m.width}x${toString m.height}@${toString m.refreshRate}";
      position = "${toString m.x}x${toString m.y}";
    in
    "${m.name},${resolution},${position},${toString m.scale}"
  ) config.nocturne.wayland.docked-monitors;

  # ===Hyprdock Script===
  # Disgusting script to disable monitors when docking
  hyprdock =
    let
      # String that grep uses to check if any of the docked monitors are connected
      grepString =
        let
          start = "^" + (builtins.head config.nocturne.wayland.docked-monitors).name;
          list = builtins.tail config.nocturne.wayland.docked-monitors;
        in
        lib.lists.foldr (x: acc: acc + "\\|^" + x.name) start list;

      # String that set docked monitors
      dmonString =
        let
          start = "hyprctl keyword monitor " + builtins.head docked-monitors;
          list = builtins.tail docked-monitors;
        in
        lib.lists.foldr (x: acc: acc + "\n      hyprctl keyword monitor " + x) start list;

      # String that enable default monitors
      monString =
        let
          start = "hyprctl keyword monitor " + builtins.head monitors;
          list = builtins.tail monitors;
        in
        lib.lists.foldr (x: acc: acc + "\n    hyprctl keyword monitor " + x) start list;

      # String that disable default monitors
      dismonString =
        let
          start =
            "hyprctl keyword monitor " + (builtins.head config.nocturne.wayland.monitors).name + ",disable";
          list = map (m: "${m.name},disable") (builtins.tail config.nocturne.wayland.monitors);
        in
        lib.lists.foldr (x: acc: acc + "\n      hyprctl keyword monitor " + x) start list;
    in
    pkgs.writeShellScriptBin "hyprdock" ''
      monitorIsDocked="$(${pkgs.wlr-randr}/bin/wlr-randr | grep '${grepString}')"
      case $1 in
        -dock)
          if [ -n "$monitorIsDocked" ]; then
            ${dismonString}
            ${dmonString}
          fi ;;
        -undock)
          ${monString} ;;
        *) echo "unknown parameter" ;; 
      esac
    '';
in
{
  config = {
    # ===Packages Needed===
    home.packages = [
      hyprdock
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
      #package = inputs.hyprland.packages.${pkgs.system}.hyprland;
      settings = {
        "$terminal" = "${term-cfg.exec}";
        "$editor" = "${ed-cfg.exec}";
        "$fileManager" = "dolphin";
        "$menu-drun" = menu-drun;
        "$menu-run" = menu-run;
        "$menu-window" = menu-window;
        "$MOD" = "SUPER";

        env = [ "XCURSOR_SIZE,24" ];
        monitor = monitors ++ [ ",preferred,auto,auto" ];
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
          "float, class:^(pavucontrol)$, title:^(Volume Control)$"
          "size 80% 85%, class:^(pavucontrol)$, title:^(Volume Control)$"
          "center, class:^(pavucontrol)$, title:^(Volume Control)$"

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
            "$MOD, code:61, exec, $menu-window"
            "$MOD, B, exec, killall '.waybar-wrapped' || waybar"
            "$MOD_SHIFT, C, killactive, "
            "$MOD, C, killactive, "
            "$MOD_SHIFT, Q, exit,"
            ## Logout (semi-colon)
            "$MOD_CTRL, code:47, exec, ${config.nocturne.wayland.logout.exec}"
            "$MOD, code:47, exec, ${config.nocturne.wayland.logout.exec}"

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
          ", XF86AudioLowerVolume, exec, ${pkgs.pamixer}/bin/pamixer -d 1"
          ", XF86AudioRaiseVolume, exec, ${pkgs.pamixer}/bin/pamixer -i 1"
          ", XF86AudioMute, exec, ${pkgs.pamixer}/bin/pamixer -t"
          ", XF86MonBrightnessUp, exec, ${pkgs.brightnessctl}/bin/brightnessctl s +1%"
          ", XF86MonBrightnessDown, exec, ${pkgs.brightnessctl}/bin/brightnessctl s 1%-"
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
            ",switch:on:Lid Switch,exec,${hyprdock}/bin/hyprdock -dock"
            ",switch:off:Lid Switch,exec,${hyprdock}/bin/hyprdock -undock"
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
  };
}
