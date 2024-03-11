{ config, pkgs, inputs, ... }: {
  programs.git = let cfg = config.nocturne.cli.git;
  in {
    enable = true;
    userEmail = cfg.userEmail;
    userName = cfg.userName;
  };
}
