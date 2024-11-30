{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.nocturne.graphical.rofi;
  way-cfg = config.nocturne.wayland.menu.name;
in
{
  config = lib.mkMerge [
    (lib.mkIf (way-cfg == "rofi") {
      # Warnings
      warnings =
        lib.optionals ((way-cfg == "rofi-wayland") && (cfg.enable == false)) [
          "rofi is set as the wayland application launcher but is not enabled"
        ]
        ++ lib.optionals ((way-cfg != "rofi-wayland") && (cfg.enable == true)) [
          "rofi is enabled but not set as the wayland application launcher"
        ];

      home.packages = [
        pkgs.nerd-fonts.jetbrains-mono
        pkgs.rofi-wayland
      ];
      nocturne.wayland.menu = {
        promptSwitch = "-p";
        exec = "${pkgs.rofi-wayland}/bin/rofi -show drun";
        exec-run = "${pkgs.rofi-wayland}/bin/rofi -show run";
        exec-dmenu = "${pkgs.rofi-wayland}/bin/rofi -dmenu -i";
      };
      programs.rofi = {
        enable = true;
        package = pkgs.rofi-wayland;
        font = "JetBrainsMono Nerd Font 12";
        extraConfig = {
          modi = "run,drun,window";
          drun-display-format = "{name}";
          # sidebar-mode = true;
          matching = "fuzzy";
          scroll-method = 0;
          disable-history = false;
          show-icons = true;

          display-drun = "Application";
          display-run = "Command";
          display-window = "Window";

          kb-mode-complete = "";
          kb-remove-to-eol = "";
          kb-remove-char-back = "BackSpace,Shift+BackSpace";
          kb-accept-entry = "Return,KP_Enter";
          kb-primary-paste = "Alt-v";
          kb-row-up = "Up,Control-p,Control-k";
          kb-row-down = "Down,Control-n,Control-j";
          kb-row-left = "Control-h";
          kb-row-right = "Control-l";
        };
        theme =
          let
            inherit (config.lib.formats.rasi) mkLiteral;
            colors = {
              bg = cfg.bg;
              bg-selection = cfg.bg-selection;
              fg = cfg.fg;
              fg-selection = cfg.fg-selection;
              fg-placeholder = cfg.fg-placeholder;
              fg-urgent = cfg.fg-urgent;
              fg-active = cfg.fg-active;
              border-color = cfg.border-color;
            };
          in
          {
            "*" = {
              bg = mkLiteral "#${colors.bg}";
              bg-selection = mkLiteral "#${colors.bg-selection}";
              fg = mkLiteral "#${colors.fg}";
              fg-selection = mkLiteral "#${colors.fg-selection}";
              fg-placeholder = mkLiteral "#${colors.fg-placeholder}";
              fg-urgent = mkLiteral "#${colors.fg-urgent}";
              fg-active = mkLiteral "#${colors.fg-active}";
              border-color = mkLiteral "#${colors.border-color}";
            };
            "window" = {
              width = mkLiteral "60%";
              border = mkLiteral "2px";
              border-color = mkLiteral "@border-color";
              border-radius = mkLiteral "8px";
              background-color = mkLiteral "@bg";
            };
            "mainbox" = {
              enabled = true;
              background-color = mkLiteral "transparent";
              children = mkLiteral "[inputbar,listview]";
            };
            "inputbar" = {
              spacing = mkLiteral "4px";
              background-color = mkLiteral "@bg";
              padding = mkLiteral "6px";
              children = mkLiteral "[prompt, entry]";
            };
            "prompt" = {
              padding = mkLiteral "6px";
              border-radius = mkLiteral "8px";
              background-color = mkLiteral "@bg-selection";
              text-color = mkLiteral "@fg";
            };
            "entry" = {
              padding = mkLiteral "6px";
              border-radius = mkLiteral "8px";
              background-color = mkLiteral "@bg-selection";
              text-color = mkLiteral "@fg";
              placeholder = "Search";
              placeholder-color = mkLiteral "@fg-placeholder";
            };
            "listview" = {
              columns = 2;
              lines = 7;
              fixed-height = true;
              fixed-columns = false;
              padding = mkLiteral "6px";
              background-color = mkLiteral "@bg";
            };
            "element" = {
              spacing = mkLiteral "4px";
              background-color = mkLiteral "@bg";
              padding = mkLiteral "6px";
              border-radius = mkLiteral "8px";
              text-color = mkLiteral "@fg";
            };
            "element normal" = {
              text-color = mkLiteral "@fg";
            };
            "element urgent" = {
              text-color = mkLiteral "@fg-urgent";
            };
            "element active" = {
              text-color = mkLiteral "@fg-active";
            };
            "element selected.normal" = {
              text-color = mkLiteral "@fg-selection";
              background-color = mkLiteral "@bg-selection";
            };
            "element selected.urgent" = {
              text-color = mkLiteral "@fg-urgent";
              background-color = mkLiteral "@bg-selection";
            };
            "element selected.active" = {
              text-color = mkLiteral "@fg-active";
              background-color = mkLiteral "@bg-selection";
            };
            "element-icon" = {
              padding = mkLiteral "4px";
              text-color = mkLiteral "inherit";
              background-color = mkLiteral "transparent";
            };
            "element-text" = {
              padding = mkLiteral "4px";
              text-color = mkLiteral "inherit";
              background-color = mkLiteral "transparent";
            };
          };
      };
    })
  ];
}
