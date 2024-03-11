{ lib, config, pkgs, inputs, ... }:

let cfg = config.nocturne.graphical.discord;
in { config = lib.mkIf cfg.enable { home.packages = with pkgs; [ vesktop ]; }; }
