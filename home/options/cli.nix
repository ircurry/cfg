{ lib, ... }:
{
  options.nocturne.cli = {
    # ===Abstract Options===
    editor = {
      name = lib.mkOption {
        type = lib.types.enum [
          "vim"
          "emnw"
        ];
        default = "vim";
        description = "Default cli editor";
      };
      follow = lib.mkOption {
        type = lib.types.enum [
          "cli"
          "wayland"
        ];
        default = "wayland";
        description = "Use graphical or terminal editor";
      };
      exec = lib.mkOption { type = lib.types.str; };
      exec-start = lib.mkOption { type = lib.types.str; };
    };
    shell = {
      name = lib.mkOption {
        type = lib.types.enum [
          "zsh"
          "fish"
        ];
        default = "fish";
        description = "Interactive Shell";
      };
      exec = lib.mkOption { type = lib.types.str; };
    };

    # ===Program Options===
    amfora.enable = lib.mkEnableOption "Enable the Amfora Gemini browser";
    ani-cli.enable = lib.mkEnableOption "Enable ani-cli";
    conversion.enable = lib.mkEnableOption "Enable conversion tools";
    direnv.enable = lib.mkEnableOption "Enable direnv";
    file-convert.enable = lib.mkEnableOption "Enable file conversion tools";
    git = {
      userName = lib.mkOption {
        type = lib.types.str;
        default = "Ian Curran";
        description = "Default name for git";
      };
      userEmail = lib.mkOption {
        type = lib.types.str;
        default = "icurran@protonmail.com";
        description = "Default email for git";
      };
    };
    lf.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable lf File Manager";
    };
    mpd.enable = lib.mkEnableOption "Enable Music Player Daemon and NCMPCPP";
    phetch.enable = lib.mkEnableOption "Enable Phetch";
    youtube = {
      enable = lib.mkEnableOption "Enable youtube scripts and programs";
      ytfzf.enable = lib.mkEnableOption "Enable ytfzf and scripts around it" // {
        default = true;
      };
      youtubeScripts.enable = lib.mkEnableOption "Enable YouTube scripts" // {
        default = true;
      };
    };
  };
}
