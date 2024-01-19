{ lib, ... }: {
  options.nocturne.cli = {
    amfora.enable = lib.mkEnableOption "Enable the Amfora Gemini browser";
    ani-cli.enable = lib.mkEnableOption "Enable ani-cli";
    editor = {
      name = lib.mkOption {
        type = lib.types.enum [ "vim" "emnw" ];
        default = "vim";
        description = "Default cli editor";
      };
      exec = lib.mkOption {
        type = lib.types.str;
      };
      exec-start = lib.mkOption {
        type = lib.types.str;
      };
    };
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
    phetch.enable = lib.mkEnableOption "Enable Phetch";
    scripts = {
      youtubeScripts.enable = lib.mkEnableOption "Enable YouTube scripts";
    };
    ytfzf.enable = lib.mkEnableOption "Enable ytfzf and scripts around it";
  };
}
