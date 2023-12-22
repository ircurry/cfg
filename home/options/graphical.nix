{ lib, ... }: {
  options.nocturne.graphical = {
    alacritty.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable Alacritty";
    };
    brave.enable = lib.mkEnableOption "Enable Brave Browser";
    firefox.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable Firefox";
    };
    tor-browser.enable = lib.mkEnableOption "Enable the Tor Browser";
  };
}
