{ config, lib, pkgs, ... }:
let cfg = config.nocturne.wayland.lock;
in {
  config = lib.mkIf (cfg.name == "swaylock") {
    home.packages = [ pkgs.nerdfonts pkgs.swaylock-effects ];
    nocturne.wayland.lock.exec = "${lib.getExe pkgs.swaylock-effects}";
    xdg.configFile."swaylock/config" = {
      enable = true;
      text = let
        bg = config.nocturne.wayland.swaylock-effects.bg;
        fg = config.nocturne.wayland.swaylock-effects.fg;
        bg-inside = config.nocturne.wayland.swaylock-effects.bg-inside;
        ring = config.nocturne.wayland.swaylock-effects.ring;
        # Is base0F for nord aurora
        key-press = config.nocturne.wayland.swaylock-effects.key-press;
        fg-ring-clear = config.nocturne.wayland.swaylock-effects.fg-ring-clear;
        fg-ver = config.nocturne.wayland.swaylock-effects.fg-ver;
        # Is base0F for nord aurora
        ring-ver = config.nocturne.wayland.swaylock-effects.ring-ver;
        fg-wrong = config.nocturne.wayland.swaylock-effects.fg-wrong;
        ring-wrong = config.nocturne.wayland.swaylock-effects.ring-wrong;
        fg-caps = config.nocturne.wayland.swaylock-effects.fg-caps;
        key-press-caps =
          config.nocturne.wayland.swaylock-effects.key-press-caps;
      in ''
        ignore-empty-password
        font="Fira Sans Semibold"

        clock
        timestr=%T
        datestr=%a, %e of %b

        indicator
        indicator-radius=125

        # Background Color
        color=${bg}

        # Normal Colors
        text-color=${fg}
        inside-color=${bg-inside}
        line-color=${bg}
        separator-color=${bg}
        ring-color=${ring}
        key-hl-color=${key-press}
        bs-hl-color=${ring}

        # Cleared Colors
        text-clear-color=${fg-ring-clear}
        inside-clear-color=${bg-inside}
        line-clear-color=${bg}
        ring-clear-color=${fg-ring-clear}

        # Verifying Colors
        text-ver-color=${fg-ver}
        inside-ver-color=${bg-inside}
        line-ver-color=${bg}
        ring-ver-color=${ring-ver}

        # Wrong Colors
        text-wrong-color=${fg-wrong}
        inside-wrong-color=${bg-inside}
        line-wrong-color=${bg}
        ring-wrong-color=${ring-wrong}

        # Caps-lock Colors
        text-caps-lock-color=${fg-caps}
        inside-caps-lock-color=${bg-inside}
        line-caps-lock-color=${bg}
        ring-caps-lock-color=${ring}
        caps-lock-key-hl-color=${key-press-caps}
        caps-lock-bs-hl-color=${ring}
      '';
    };
  };
}
