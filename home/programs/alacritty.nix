{ config, lib, pkgs, ... }:

let
  cfg = config.nocturne.graphical.alacritty;
  way-cfg = config.nocturne.wayland.terminal;
in
{
  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      programs.alacritty = {
        enable = true;
        settings = {
          window = {
            padding = {
              x = 4;
              y = 4;
            };
            opacity = 0.85;
            title = "Alacritty";
            class = {
              instance = "Alacritty";
              general = "Alacritty";
            };
          };
          
          font = {
            normal.family = "JetBrainsMono Nerd Font";
            bold.family = "JetBrainsMono Nerd Font";
            bold.style = "Bold";
            size = 10;
          };
          
          keyboard.bindings = [
            {
              key = "V";
              mods = "Alt";
              action = "Paste";
            }
            {
              key = "C";
              mods = "Alt";
              action = "Copy";
            }
            {
              key = "K";
              mods = "Alt|Control";
              action = "Increasefontsize";
            }
            {
              key = "J";
              mods = "Alt|Control";
              action = "Decreasefontsize";
            }
          ];
          colors = {
            primary = {
              background = "0x2e3440";
              foreground = "0xd8dee9";
            };
            cursor = {
              cursor = "0xd8dee9";
              text = "0x2e3440";
            };
            normal = {
              black = "0x3b4252";
              red = "0xbf616a";
              green = "0xa3be8c";
              yellow = "0xebcb8b";
              blue = "0x81a1c1";
              magenta = "0xb48ead";
              cyan = "0x88c0d0";
              white = "0xe5e9f0";
            };
            bright = {
              black = "0x4c566a";
              red = "0xbf616a";
              green = "0xa3be8c";
              yellow = "0xebcb8b";
              blue = "0x81a1c1";
              magenta = "0xb48ead";
              cyan = "0x8fbcbb";
              white = "0xeceff4";
            };
          };
        };
      };
    })
    (lib.mkIf (way-cfg.name == "alacritty") {
      assertions = [
        {
          assertion = cfg.enable == true;
          message = "alacritty is set as the default terminal emulator on wayland but is not ebabled";
        }
      ];
      nocturne.wayland.terminal.exec = "${lib.getExe pkgs.alacritty}";
      nocturne.wayland.terminal.exec-start = null;
    })
  ];
}
