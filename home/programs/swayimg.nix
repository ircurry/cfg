{ config, lib, ... }:
let
  wcfg = config.nocturne.wayland.image;
  scfg = config.nocturne.graphical.swayimg;
  simg = config.programs.swayimg;
  inherit (config.nocturne.wayland.wallpaper) exec-change;
  inherit (scfg)
    bg
    tile-bg
    tile-border
    fg
    ;
in
{
  config = lib.mkIf (wcfg.name == "swayimg") {
    programs.swayimg = {
      enable = true;
      settings = {
        general = {
          mode = "viewer";
          size = "parent";
          decoration = "no";
        };
        viewer = {
          window = "#" + bg + "ff";
          transparency = "grid";
          scale = "fit";
          keep_zoom = "no";
          position = "center";
          fixed = "yes";
          antialiasing = "mks13";
          slideshow = "no";
          slideshow_time = 3;
          history = 2;
          preload = 1;
        };
        gallery = {
          size = 200;
          pstore = "yes";
          fill = "yes";
          cache = 200;
          antialiasing = "mks13";
          window = "#" + bg + "ff";
          background = "#" + tile-bg + "ff";
          border = "#" + tile-border + "ff";
          shadow = "#00000000";
        };
        list = {
          order = "alpha";
          loop = "yes";
          recursive = "no";
          all = "yes";
        };
        font = {
          name = "JetBrainsMono Nerd Font";
          size = 14;
          color = "#" + fg + "ff";
          background = "#00000000";
          shadow = "#00000000";
        };
        info = {
          show = "no";
          info_timeout = 0;
          status_timeout = 3;
        };
        "info.viewer" = {
          top_left = "+name,+dir,+format,+filesize,+imagesize,+exif";
          top_right = "index";
          bottom_left = "scale,frame";
          bottom_right = "status";
        };
        "info.gallery" = {
          top_left = "none";
          top_right = "none";
          bottom_left = "none";
          bottom_right = "name,status";
        };
        "keys.viewer" = {
          # ===1st Row===
          "1" = "none";
          "2" = "none";
          "3" = "none";
          "4" = "none";
          "5" = "none";
          "6" = "none";
          "7" = "none";
          "8" = "none";
          "9" = "none";
          "0" = "none";
          "Equal" = "zoom +10";
          "Plus" = "zoom +10";
          "Backspace" = "zoom optimal";

          # ===2nd Row===
          "Tab" = "mode";
          "q" = "exit";
          "w" = "exec ${exec-change} \"%\"";
          "Shift+w" = "zoom optimal";
          "e" = "next_file";
          "Shift+e" = "next_dir";
          "r" = "rotate_left";
          "Shift+r" = "rotate_right";
          "t" = "flip_vertical";
          "Shift+t" = "flip_horizontal";
          "y" = "none";
          "u" = "none";
          "i" = "info";
          "Shift+i" = "info";
          "o" = "none";
          "p" = "prev_frame";
          "Minus" = "zoom -10";

          # ===3rd Row===
          "a" = "animation";
          "Shift+a" = "antialiasing";
          "s" = "zoom +10";
          "Shift+s" = "scale";
          "d" = "zoom -10";
          "Shift+d" = "skip_file";
          "f" = "fullscreen";
          "g" = "reload";
          "h" = "step_left 2";
          "Shift+h" = "step_left 15";
          "j" = "step_down 2";
          "Shift+j" = "step_down 15";
          "k" = "step_up 2";
          "Shift+k" = "step_up 15";
          "l" = "step_right 2";
          "Shift+l" = "step_right 15";
          ";" = "none";
          "Return" = "mode";

          # ===4th Row===
          "z" = "keep_zoom";
          "x" = "none";
          "c" = "none";
          "v" = "zoom fit";
          "b" = "prev_file";
          "Shift+b" = "prev_dir";
          "n" = "next_frame";
          "Shift+n" = "prev_frame";
          "m" = "mode";
          "Comma" = "first_file";
          "Period" = "last_file";
          "Slash" = "none";
          "Shift+Question" = "help";

          # ===5th Row===
          "Space" = "next_file";
          "Left" = "prev_file";
          "Down" = "next_dir";
          "Up" = "prev_dir";
          "Right" = "next_file";
          "Home" = "first_file";
          "End" = "last_file";
        };
        "keys.gallery" = {
          # ===1st Row===
          "1" = "none";
          "2" = "none";
          "3" = "none";
          "4" = "none";
          "5" = "none";
          "6" = "none";
          "7" = "none";
          "8" = "none";
          "9" = "none";
          "0" = "none";
          "Equal" = "none";
          "Plus" = "none";
          "Backspace" = "none";

          # ===2nd Row===
          "Tab" = "mode";
          "q" = "exit";
          "w" = "exec ${exec-change} \"%\"";
          "e" = "none";
          "r" = "none";
          "t" = "none";
          "y" = "none";
          "u" = "page_up";
          "i" = "info";
          "Shift+i" = "info";
          "o" = "none";
          "p" = "none";
          "Minus" = "none";

          # ===3rd Row===
          "a" = "antialiasing";
          "Shift+a" = "antialiasing";
          "s" = "none";
          "d" = "page_down";
          "Shift+d" = "skip_file";
          "f" = "fullscreen";
          "g" = "reload";
          "h" = "step_left";
          "Shift+h" = "prev_file";
          "j" = "step_down";
          "k" = "step_up";
          "l" = "step_right";
          "Shift+l" = "next_file";
          ";" = "none";
          "Return" = "mode";

          # ===4th Row===
          "z" = "none";
          "x" = "none";
          "c" = "none";
          "v" = "none";
          "b" = "none";
          "n" = "next_file";
          "Shift+n" = "prev_file";
          "m" = "mode";
          "Comma" = "first_file";
          "Period" = "last_file";
          "Slash" = "none";
          "Shift+Question" = "help";

          # ===5th Row===
          "Space" = "next_file";
          "Left" = "step_left";
          "Down" = "step_down";
          "Up" = "step_up";
          "Right" = "step_right";
          "Home" = "first_file";
          "End" = "last_file";
        };
      };
    };
    nocturne.wayland.image = {
      exec = "${simg.package}/bin/swayimg";
      exec-dir = "${simg.package}/bin/swayimg --config \"list.all=yes\"";
    };
  };
}
