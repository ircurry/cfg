{ config, lib, pkgs, ... }: let
  cfg = config.nocturne.cli.direnv;
in {
  config = lib.mkIf cfg.enable {
    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
    home.sessionVariables.DIRENV_LOG_FORMAT = "";
  };
}
