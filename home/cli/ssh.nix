{
  config,
  lib,
  pkgs,
  ...
}:

{
  config = {
    services.ssh-agent.enable = true;
    programs.ssh = {
      enable = true;
      enableDefaultConfig = false;
      matchBlocks = {
        "*" = {
          addKeysToAgent = "yes";
          hashKnownHosts = true;
        };
        "github.com" = {
          host = "github.com";
          user = "git";
          identityFile = "~/.ssh/id_github";
        };
        "gitlab.com" = {
          host = "gitlab.com";
          user = "git";
          identityFile = "~/.ssh/id_gitlab";
        };
        "codeburg.org" = {
          host = "codeburg.org";
          user = "git";
          identityFile = "~/.ssh/id_codeburg";
        };
      };
    };
  };
}
