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
      addKeysToAgent = "yes";
      hashKnownHosts = true;
      matchBlocks = {
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
      };
    };
  };
}
