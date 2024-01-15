{ lib, ... }: {
  options.nocturne.wayland = {
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
