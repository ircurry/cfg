{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.nocturne.graphical.alacritty;
  way-cfg = config.nocturne.wayland.terminal;
  shell-cfg = config.nocturne.cli.shell;
  bg = config.nocturne.graphical.alacritty.bg; # ----
  fg = config.nocturne.graphical.alacritty.fg; # ++
  black = config.nocturne.graphical.alacritty.black; # ---
  red = config.nocturne.graphical.alacritty.red; # red
  yellow = config.nocturne.graphical.alacritty.yellow; # yellow
  green = config.nocturne.graphical.alacritty.green; # green
  cyan = config.nocturne.graphical.alacritty.cyan; # aqua/cyan
  blue = config.nocturne.graphical.alacritty.blue; # blue
  magenta = config.nocturne.graphical.alacritty.magenta; # purple
  white = config.nocturne.graphical.alacritty.white; # +
  bright-black = config.nocturne.graphical.alacritty.bright-black; # -
  bright-red = config.nocturne.graphical.alacritty.bright-red; # red
  bright-yellow = config.nocturne.graphical.alacritty.bright-yellow; # yellow
  bright-green = config.nocturne.graphical.alacritty.bright-green; # green
  bright-cyan = config.nocturne.graphical.alacritty.bright-cyan; # ~, normally aqua/cyan
  bright-blue = config.nocturne.graphical.alacritty.bright-blue; # blue
  bright-magenta = config.nocturne.graphical.alacritty.bright-magenta; # purple
  bright-white = config.nocturne.graphical.alacritty.bright-white; # +++
in
{
  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      programs.alacritty = {
        enable = true;
        settings = {
          shell = {
            program = shell-cfg.exec;
          };
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
              background = "0x${bg}";
              foreground = "0x${fg}";
            };
            cursor = {
              cursor = "0x${fg}";
              text = "0x${bg}";
            };
            normal = {
              black = "0x${black}";
              red = "0x${red}";
              green = "0x${green}";
              yellow = "0x${yellow}";
              blue = "0x${blue}";
              magenta = "0x${magenta}";
              cyan = "0x${cyan}";
              white = "0x${white}";
            };
            bright = {
              black = "0x${bright-black}";
              red = "0x${bright-red}";
              green = "0x${bright-green}";
              yellow = "0x${bright-yellow}";
              blue = "0x${bright-blue}";
              magenta = "0x${bright-magenta}";
              cyan = "0x${bright-cyan}";
              white = "0x${bright-white}";
            };
          };
        };
      };
    })
    (lib.mkIf (way-cfg.name == "alacritty") {
      assertions = [
        {
          assertion = cfg.enable == true;
          message = "alacritty is set as the default terminal emulator on wayland but is not enabled";
        }
      ];
      nocturne.wayland.terminal.exec = "${lib.getExe pkgs.alacritty}";
      nocturne.wayland.terminal.exec-start = null;
      nocturne.wayland.terminal.exec-center = "${lib.getExe pkgs.alacritty} --class=center -e";
    })
  ];
}
