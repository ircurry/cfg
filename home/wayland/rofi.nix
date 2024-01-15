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
          base00 = "2e3440";
          base01 = "3b4252";
          base02 = "434c5e";
          base03 = "4c566a";
          base04 = "d8dee9";
          base05 = "e5e9f0";
          base06 = "eceff4";
          base08 = "bf616a";
          base09 = "d08770";
          base0A = "ebcb8b";
          base0B = "a3be8c";
          base0C = "88c0d0";
          base0D = "81a1c1";
          base0E = "b48ead";
          base0F = "5e81ac";
        };
      in {
        "*" = {
          bg = mkLiteral "#${colors.base00}";
          bg-selection = mkLiteral "#${colors.base01}";
          fg = mkLiteral "#${colors.base05}";
          fg-selection = mkLiteral "#${colors.base04}";
          fg-placeholder = mkLiteral "#${colors.base03}";
          fg-urgent = mkLiteral "#${colors.base09}";
          fg-active = mkLiteral "#${colors.base0C}";
          border-color = mkLiteral "#${colors.base0F}";
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
        "element normal.normal" = {
          text-color = mkLiteral "@fg";
        };
        "element normal.urgent" = {
          text-color = mkLiteral "@fg-urgent";
        };
        "element normal.active" = {
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
