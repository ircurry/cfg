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
      pkgs.nerd-fonts.jetbrains-mono
      pkgs.libnotify
    ];
    nocturne.wayland.notification.exec-volup = "${lib.getExe pkgs.pamixer} -i 1";
    nocturne.wayland.notification.exec-voldown = "${lib.getExe pkgs.pamixer} -d 1";
    nocturne.wayland.notification.exec-volmute = "${lib.getExe pkgs.pamixer} -t";
    nocturne.wayland.notification.exec-brightup = "${lib.getExe pkgs.brightnessctl} s +1%";
    nocturne.wayland.notification.exec-brightdown = "${lib.getExe pkgs.brightnessctl} s 1%-";
    nocturne.wayland.startup = [
      {
        exec = "mako";
        packages = [ pkgs.mako ];
      }
    ];
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
