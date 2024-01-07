{ lib, ... }: {
  options.nocturne.themes = {
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
