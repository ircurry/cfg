{ lib, ... }: {
  options.nocturne.themes = {
    theme = lib.mkOption {
      type = lib.types.enum [ "basic-dark" "gruvbox-dark-medium" "nord-aurora" ];
      default = "nord-aurora";
      description = "Which theme for the system to use";
    };
    colors = {
      base00 = lib.mkOption {
        type = lib.types.str;
      };
      base01 = lib.mkOption {
        type = lib.types.str;
      };
      base02 = lib.mkOption {
        type = lib.types.str;
      };
      base03 = lib.mkOption {
        type = lib.types.str;
      };
      base04 = lib.mkOption {
        type = lib.types.str;
      };
      base05 = lib.mkOption {
        type = lib.types.str;
      };
      base06 = lib.mkOption {
        type = lib.types.str;
      };
      base07 = lib.mkOption {
        type = lib.types.str;
      };
      base08 = lib.mkOption {
        type = lib.types.str;
      };
      base09 = lib.mkOption {
        type = lib.types.str;
      };
      base0A = lib.mkOption {
        type = lib.types.str;
      };
      base0B = lib.mkOption {
        type = lib.types.str;
      };
      base0C = lib.mkOption {
        type = lib.types.str;
      };
      base0D = lib.mkOption {
        type = lib.types.str;
      };
      base0E = lib.mkOption {
        type = lib.types.str;
      };
      base0F = lib.mkOption {
        type = lib.types.str;
      };
    };
    gtk.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable GTK";
    };
    qt.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable qt";
    };
  };
}
