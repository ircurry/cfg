{
  config,
  pkgs,
  inputs,
  ...
}:
{
  programs.git =
    let
      cfg = config.nocturne.cli.git;
    in
    {
      enable = true;
      settings.user = {
        email = cfg.userEmail;
        name = cfg.userName;
      };
    };
}
