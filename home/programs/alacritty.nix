{ config, lib, pkgs, ... }:

let
  cfg = config.nocturne.graphical.alacritty;
in
{
  options.nocturne.graphical.alacritty = {
    enable = lib.mkEnableOption "Whether to enable alacrittys for user";
  };

  config = lib.mkIf cfg.enable {
    programs.alacritty = {
      enable = true;
      settings = {
        window = {
          padding = {
            x = 4;
            y = 4;
          };
          opacity = 1.0;
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

        key_bindings = [
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
            key = "J";
            mods = "Alt|Control";
            action = "Increasefontsize";
          }
          {
            key = "K";
            mods = "Alt|Control";
            action = "Decreasefontsize";
          }
        ];
        
      };
    };
  };
}
