{ lib, ... }: {
  options.nocturne.wayland = {
    screen-shot = {
      name = lib.mkOption {
        type = lib.types.enum [ "grim-slurp" ];
        default = "grim-slurp";
        example = "grim-slurp";
        description = "Which screenshot program to use";
      };
    };
  };
}
