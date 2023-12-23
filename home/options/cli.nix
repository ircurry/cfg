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
    scripts = {
      youtubeScripts.enable = lib.mkEnableOption "Enable YouTube scripts";
    };
  };
}
