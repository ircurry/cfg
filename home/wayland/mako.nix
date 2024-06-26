{
  lib,
  config,
  pkgs,
  ...
}:
let
  cfg = config.nocturne.wayland.notification.daemon;
in
{
  config = lib.mkIf (cfg == "mako") {
    home.packages = [
      pkgs.nerdfonts
      pkgs.libnotify
    ];
    nocturne.wayland.notification.exec-start = "${lib.getExe pkgs.mako}";
    services.mako =
      let
        bg = config.nocturne.wayland.mako.bg;
        fg = config.nocturne.wayland.mako.fg;
        border-color = config.nocturne.wayland.mako.border-color;
        progress-color = config.nocturne.wayland.mako.progress-color;
      in
      {
        enable = true;
        defaultTimeout = 7000;
        font = "JetBrainsMono Nerd Font 10";
        backgroundColor = "#" + bg;
        textColor = "#" + fg;
        borderColor = "#" + border-color;
        borderRadius = 8;
        borderSize = config.nocturne.wayland.mako.borderSize;
        progressColor = "#" + progress-color;
      };
  };
}
