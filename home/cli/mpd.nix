{ config, lib, ... }:
let
  cfg = config.nocturne.cli.mpd;
  musicDir = config.xdg.userDirs.music;
in {
  config = lib.mkIf cfg.enable {
    services.mpd = {
      enable = true;
      musicDirectory = musicDir;
      network = {
        startWhenNeeded = false;
        listenAddress = "127.0.0.1";
        port = 6600;
      };
      extraConfig = ''
        audio_output {
          type "pipewire"
          name "PipeWire"
          auto_resample "no"
          use_mmap "yes"
        }

        audio_output {
          type                    "fifo"
          name                    "fifo"
          path                    "/tmp/mpd.fifo"
          format                  "44100:16:2"
        }

        auto_update "yes"
      '';
    };

    programs.ncmpcpp = {
      enable = true;
      mpdMusicDir = musicDir;
      bindings = [
        {
          key = "+";
          command = "show_clock";
        }
        {
          key = "0";
          command = "volume_up";
        }
        {
          key = "9";
          command = "volume_down";
        }
        {
          key = "j";
          command = "scroll_down";
        }
        {
          key = "k";
          command = "scroll_up";
        }
        {
          key = ":";
          command = "page_up";
        }
        {
          key = "?";
          command = "page_down";
        }
        {
          key = "h";
          command = "previous_column";
        }
        {
          key = "l";
          command = "next_column";
        }
        {
          key = "n";
          command = "next_found_item";
        }
        {
          key = ";";
          command = "previous_found_item";
        }
        {
          key = "J";
          command = "move_sort_order_down";
        }
        {
          key = "K";
          command = "move_sort_order_down";
        }
        {
          key = "h";
          command = "jump_to_parent_directory";
        }
        {
          key = "l";
          command = "enter_directory";
        }
        {
          key = "l";
          command = "run_action";
        }
        {
          key = "l";
          command = "play_item";
        }
        {
          key = "m";
          command = "show_media_library";
        }
        {
          key = "t";
          command = "show_tag_editor";
        }
        {
          key = "v";
          command = "show_visualizer";
        }
        {
          key = ",";
          command = "move_home";
        }
        {
          key = ".";
          command = "move_end";
        }
        {
          key = "R";
          command = "update_database";
        }
        {
          key = "s";
          command = "reset_search_engine";
        }
        {
          key = "s";
          command = "stop";
        }
        {
          key = "f";
          command = "show_browser";
        }
        {
          key = "f";
          command = "change_browse_mode";
        }
        {
          key = "d";
          command = "delete_playlist_items";
        }
        {
          key = "P";
          command = "show_playlist";
        }
      ];
      settings = {
        ncmpcpp_directory = "${config.xdg.configHome}/ncmpcpp";
        ignore_leading_the = true;
        user_interface = "alternative";
        colors_enabled = "yes";

        playlist_editor_display_mode = "columns";
        search_engine_display_mode = "columns";
        browser_display_mode = "columns";
        playlist_display_mode = "columns";

        now_playing_prefix = "> ";

        visualizer_data_source = "/tmp/mpd.fifo";
        visualizer_output_name = "my_fifo";
        visualizer_in_stereo = "yes";
        #visualizer_type = "spectrum";
        visualizer_type = "ellipse";
        visualizer_look = "+|";
      };
    };
  };
}
