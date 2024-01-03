{ lib, ... }: {
  options.nocturne.cli = {
    amfora.enable = lib.mkEnableOption "Enable the Amfora Gemini browser";
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
