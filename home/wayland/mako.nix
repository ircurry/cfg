{ lib, config, pkgs, ... }: let
  cfg = config.nocturne.wayland.notification.daemon;
in {
  config = lib.mkIf (cfg == "mako") {
    home.packages = [ pkgs.nerdfonts pkgs.libnotify ];
    nocturne.wayland.notification.exec-start = "${lib.getExe pkgs.mako}"; 
    services.mako = {
      enable = true;
      defaultTimeout = 7000;
      font = "JetBrainsMono Nerd Font 10";
      backgroundColor = "#434c5eaa";
      textColor = "#d8dee9FF";
      borderColor = "#5e81acFF";
      borderRadius = 8;
      borderSize = 2;
      progressColor = "#5e81acFF";
    };
  };
}
