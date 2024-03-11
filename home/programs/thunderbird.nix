{ config, lib, pkgs, ... }:

let cfg = config.nocturne.graphical.thunderbird;
in { config = lib.mkIf cfg.enable { home.packages = [ pkgs.thunderbird ]; }; }
