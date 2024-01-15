{ lib, ... }: {
  options.nocturne.wayland = {
    menu = {
      name = lib.mkOption {
        type = lib.types.nullOr (lib.types.enum [ "rofi-wayland" ]);
        default = "rofi-wayland";
        example = "rofi-wayland";
        description = "Which screenshot program to use";
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
  };
}
