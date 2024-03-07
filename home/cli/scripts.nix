{ config, pkgs, lib, ... }:

let
  yt-cfg = config.nocturne.cli.scripts.youtubeScripts.enable;
in {
  config = {
    home.packages = []
                    ++ lib.optionals (yt-cfg) [
                      pkgs.custom.youtubeScripts
                    ];
  };
}
