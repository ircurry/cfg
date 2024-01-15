{ config, lib, pkgs, ... }:

let
  cfg = config.nocturne.wayland.screen-shot;
  scrn = pkgs.writeShellScriptBin "noct-scrn" ''
    mkdir -p "$HOME/pix/ss"
    ssfile="$HOME/pix/ss/$(date +%Y%m%d-%s)-screenshot.png"
    ${pkgs.grim}/bin/grim - | tee "$ssfile" | ${pkgs.wl-clipboard}/bin/wl-copy && ${pkgs.libnotify}/bin/notify-send -c screenshot -i "$ssfile" "Screenshot" "Full screen screenshot taken" 2>/dev/null
  '';
  scrn-region = pkgs.writeShellScriptBin "noct-scrn-region" ''
    mkdir -p "$HOME/pix/ss"
    ssfile="$HOME/pix/ss/$(date +%Y%m%d-%s)-screenshot.png"
    ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" - | tee "$ssfile" | ${pkgs.wl-clipboard}/bin/wl-copy && ${pkgs.libnotify}/bin/notify-send -c screenshot -i "$ssfile" "Screenshot" "Selected screen screenshot taken" 2>/dev/null
  '';
in {
  config = lib.mkIf (cfg.name == "grim-slurp") {
    home.packages = [
      pkgs.grim
      pkgs.slurp
      scrn
      scrn-region
    ];
  };
}
