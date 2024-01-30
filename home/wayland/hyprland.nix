{ config, pkgs, lib, inputs, ... }: let
  menu-drun = config.nocturne.wayland.menu.drun;
  menu-run = config.nocturne.wayland.menu.run;
  menu-window = config.nocturne.wayland.menu.window;
  ed-cfg = config.nocturne.wayland.editor;
  term-cfg = config.nocturne.wayland.terminal;
in {
  config = {
    home.packages = [
      pkgs.killall
      # Audio Control
      pkgs.pavucontrol
      pkgs.swww
      pkgs.wl-clipboard
    ];
    wayland.windowManager.hyprland = {
      enable = true;
      ## Bleeding edge Hyprland
      # package = inputs.hyprland.packages."${pkgs.system}".hyprland;
      settings = {
        "$terminal" = "${term-cfg.exec}";
        "$editor" = "${ed-cfg.exec}";
        "$fileManager" = "dolphin";
        "$menu-drun" = menu-drun;
        "$menu-run" = menu-run;
        "$menu-window" = menu-window;
        "$MOD" = "SUPER";
        env = [
          "XCURSOR_SIZE,24"
        ];
        monitor= [
          ",preferred,auto,auto"
        ];
        exec-once = [
          "waybar"
          "${pkgs.swayidle}/bin/swayidle -w timeout 300 '${config.nocturne.wayland.lock.exec} -f' timeout 360 'systemctl suspend'"
          "${pkgs.swww}/bin/swww init"
          "${config.nocturne.wayland.notification.exec-start}"
          "${pkgs.networkmanagerapplet}/bin/nm-applet"
        ] ++ lib.optionals (term-cfg.exec-start != null) [
          "${term-cfg.exec-start}"
        ];
        general = {
          gaps_in = 5;
          gaps_out = 5;
          border_size = 2;
          "col.active_border" = "rgba(5e81acee) rgba(81a1c1ee) 45deg";
          "col.inactive_border" = "rgba(4c566aaa)";
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
          shadow_range = 4;
          shadow_render_power = 3;
          "col.shadow" = "rgba(1a1a1aee)";
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
          new_is_master = true;
          orientation = "top";
        };
        gestures.workspace_swipe = "off";
        misc = {
          # Anime lady hehe
          force_default_wallpaper = -1;
          enable_swallow = true;
          swallow_regex = [
            "^(Alacritty)$"
          ];
        };
        "device:at-translated-set-2-keyboard" = {
          kb_options = "ctrl:nocaps";
        };
        windowrulev2 = [
          "nomaximizerequest, class:.*"
          
          # Pavucontrol
          "float, class:^(pavucontrol)$, title:^(Volume Control)$"
          "size 80% 85%, class:^(pavucontrol)$, title:^(Volume Control)$"
          "center, class:^(pavucontrol)$, title:^(Volume Control)$"
          
          # Pavucontrol
          "float, class:^(nm-connection-editor)$, title:^(.*)$"
          "size 80% 85%, class:^(nm-connection-editor)$, title:^(.*)$"
          "center, class:^(nm-connection-editor)$, title:^(.*)$"
          
          # Center
          "float, class:^(center)$"
          "size 80% 85%, class:^(center)$"
          "center, class:^(center)$"
        ];
        bind = [
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
          "$MOD_CTRL, code:47, exec, ${config.nocturne.wayland.logout.exec}"
          "$MOD, code:47, exec, ${config.nocturne.wayland.logout.exec}"
        
          # Move focus with mainMod + arrow keys
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

          # Switch workspaces with mainMod + [0-9]
          "$MOD, 1, workspace, 1"
          "$MOD, 2, workspace, 2"
          "$MOD, 3, workspace, 3"
          "$MOD, 4, workspace, 4"
          "$MOD, 5, workspace, 5"
          "$MOD, 6, workspace, 6"
        
          # Move active window to a workspace with mainMod + SHIFT + [0-9]
          "$MOD SHIFT, 1, movetoworkspace, 1"
          "$MOD SHIFT, 2, movetoworkspace, 2"
          "$MOD SHIFT, 3, movetoworkspace, 3"
          "$MOD SHIFT, 4, movetoworkspace, 4"
          "$MOD SHIFT, 5, movetoworkspace, 5"
          "$MOD SHIFT, 6, movetoworkspace, 6"
        ] ++ lib.optionals (config.nocturne.wayland.screenshot.name != null) [
          "$MOD, S, exec, ${lib.getExe config.nocturne.wayland.screenshot.scrn}"
          "$MOD_SHIFT, S, exec, ${lib.getExe config.nocturne.wayland.screenshot.scrn-region}"
        ];
        
        binde = [
          ", XF86AudioLowerVolume, exec, ${pkgs.pamixer}/bin/pamixer -d 1"
          ", XF86AudioRaiseVolume, exec, ${pkgs.pamixer}/bin/pamixer -i 1"
          ", XF86AudioMute, exec, ${pkgs.pamixer}/bin/pamixer -t"
          ", XF86MonBrightnessUp, exec, ${pkgs.brightnessctl}/bin/brightnessctl s +1%"
          ", XF86MonBrightnessDown, exec, ${pkgs.brightnessctl}/bin/brightnessctl s 1%-"
        ];
        bindm = [
          # Move/resize windows with mainMod + LMB/RMB and dragging
          "$MOD, mouse:272, movewindow"
          "$MOD, mouse:273, resizewindow"
        ];
      };
    };
  };
}
