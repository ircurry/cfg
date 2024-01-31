{ config, pkgs, lib, ... }:
let
  cfg = config.nocturne.wayland.menu.name;
in {
  config = lib.mkIf (cfg == "rofi-wayland" ) {
    home.packages = [ pkgs.nerdfonts pkgs.rofi-wayland ];
    nocturne.wayland.menu = {
      drun = "${pkgs.rofi-wayland}/bin/rofi -show drun";
      run = "${pkgs.rofi-wayland}/bin/rofi -show run";
      window = "${pkgs.rofi-wayland}/bin/rofi -show window";
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
        kb-row-up = "Up,Control-k";
        kb-row-down = "Down,Control-j";
        kb-row-left = "Control-h";
        kb-row-right = "Control-l";
      };
      theme = let
        inherit (config.lib.formats.rasi) mkLiteral;
        colors = {
          bg = config.nocturne.graphical.rofi.bg;
          bg-selection = config.nocturne.graphical.rofi.bg-selection;
          fg = config.nocturne.graphical.rofi.fg;
          fg-selection = config.nocturne.graphical.rofi.fg-selection;
          fg-placeholder = config.nocturne.graphical.rofi.fg-placeholder;
          fg-urgent = config.nocturne.graphical.rofi.fg-urgent;
          fg-active = config.nocturne.graphical.rofi.fg-active;
          # Normally base0D
          border-color = config.nocturne.graphical.rofi.border-color;
          
          # base00 = config.nocturne.themes.colors.base00; # ----
          # base01 = config.nocturne.themes.colors.base01; # ---
          # base02 = config.nocturne.themes.colors.base02; # --
          # base03 = config.nocturne.themes.colors.base03; # -
          # base04 = config.nocturne.themes.colors.base04; # ++
          # base05 = config.nocturne.themes.colors.base05; # +
          # base06 = config.nocturne.themes.colors.base06; # +++
          # base07 = config.nocturne.themes.colors.base07; # ~
          # base08 = config.nocturne.themes.colors.base08; # red
          # base09 = config.nocturne.themes.colors.base09; # orange
          # base0A = config.nocturne.themes.colors.base0A; # yellow
          # base0B = config.nocturne.themes.colors.base0B; # green
          # base0C = config.nocturne.themes.colors.base0C; # aqua/cyan
          # base0D = config.nocturne.themes.colors.base0D; # blue
          # base0E = config.nocturne.themes.colors.base0E; # purple
          # base0F = config.nocturne.themes.colors.base0F; # brown
        };
      in {
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
  };
}
