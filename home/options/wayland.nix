{ lib, ... }: {
  options.nocturne.wayland = {
    editor = {
      name = lib.mkOption {
        type = lib.types.enum [ "emacs" ];
        default = "emacs";
        example = "emacs";
        description = "Name of the main system editor";
      };
      exec = lib.mkOption {
        type = lib.types.str;
      };
      exec-reuse = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
      };
      exec-start = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
      };
    };
    lock = {
      name = lib.mkOption {
        type = lib.types.enum [ "swaylock" ];
        default = "swaylock";
        example = "swaylock";
        description = "Which screen locking program to use";
      };
      exec = lib.mkOption {
        type = lib.types.str;
      };
    };
    logout = {
      name = lib.mkOption {
        type = lib.types.enum [ "rofi-logout" ];
        default = "rofi-logout";
        example = "rofi-logout";
        description = "Which logout program to use";
      };
      exec = lib.mkOption {
        type = lib.types.str;
      };
    };
    menu = {
      name = lib.mkOption {
        type = lib.types.enum [ "rofi-wayland" ];
        default = "rofi-wayland";
        example = "rofi-wayland";
        description = "Which menu program to use";
      };
      drun = lib.mkOption {
        type = lib.types.str;
      };
      run = lib.mkOption {
        type = lib.types.str;
      };
      window = lib.mkOption {
        type = lib.types.str;
      };
    };
    notification = {
      daemon = lib.mkOption {
        type = lib.types.enum [ "mako" ];
        default = "mako";
        example = "mako";
        description = "Which notification daemon to use";
      };
      exec-start = lib.mkOption{
        type = lib.types.str;
      };
    };
    screenshot = {
      name = lib.mkOption {
        type = lib.types.nullOr (lib.types.enum [ "grim-slurp" ]);
        default = "grim-slurp";
        example = "grim-slurp";
        description = "Which screenshot program to use";
      };
      scrn = lib.mkOption {
        type = lib.types.package;
      };
      scrn-region = lib.mkOption {
        type = lib.types.package;
      };
    };
    terminal = {
      name = lib.mkOption {
        type = lib.types.nullOr (lib.types.enum [ "alacritty" ]);
        default = "alacritty";
        example = "alacritty";
        description = "Which terminal emulator to use";
      };
      exec = lib.mkOption{
        type = lib.types.str;
      };
      exec-start = lib.mkOption{
        type = lib.types.nullOr lib.types.str;
      };
    };
  };
}
