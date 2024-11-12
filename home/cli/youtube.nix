{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.nocturne.cli.youtube;
in
{
  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      home.packages = with pkgs; [
        custom.id3
        yt-dlp
      ];
    })
    (lib.mkIf (cfg.enable && cfg.youtubeScripts.enable) {
      home.packages = with pkgs.custom; [ youtubeScripts ];
    })
    (lib.mkIf (cfg.enable && cfg.ytfzf.enable) {
      home.packages =
        let
          yt = pkgs.writeShellApplication {
            name = "yt";
            runtimeInputs = with pkgs; [
              chafa
              ytfzf
            ];
            text = ''
              if [[ "$#" -gt 0 ]]; then
                case "$1" in
                        v|vid|video) shift; ytfzf --type=video "$@";;
                        c|chan|channel) shift; ytfzf --type=channel "$@";;
                        pl|play|playlist) shift; ytfzf --type=playlist "$@";;
                        all) shift; ytfzf --type=all "$@";;
                        m|music) shift; ytfzf -m --notify-playing "$@";;
                        *) ytfzf "$@"
                esac
              else
                ytfzf && exit 0
              fi
            '';
          };
        in
        [
          yt
          pkgs.ytfzf
          pkgs.chafa
        ];

      xdg.configFile."ytfzf/conf.sh" = {
        enable = true;
        text = ''
          #!/bin/sh

          ###############
          ### General ###
          ###############
          invidious_instance="https://invidious.nerdvpn.de"
          #invidious_instance="https://vid.puffyan.us"
          #invidious_instance="https://invidious.no-logs.com"
          is_detach=0
          notify_playing=1
          search_result_type="video"
          # looping
          is_loop=1
          search_again=0
          enable_hist=1
          enable_search_hist=1
          next_page_action_shortcut="$selection_meta_key-p"

          ##############
          ### Videos ###
          ##############
          show_formats=0

          ##################
          ### Thumbnails ###
          ##################
          thumbnail_viewer="chafa"
          show_thumbnails=1

          #################
          ### Playlists ###
          #################

          ###################
          ### Audio/music ###
          ###################

          ################
          ### Channels ###
          ################

          ##############
          ### Odysee ###
          ##############
          nsfw=0
        '';
      };
    })
  ];
}
