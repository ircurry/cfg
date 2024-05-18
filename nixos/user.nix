{
  user,
  config,
  host,
  ...
}:
{
  config = {
    users.groups.plugdev = { };
    users.users."${user}" = {
      isNormalUser = true;
      description = "Ian Curran";
      extraGroups = [
        "networkmanager"
        "wheel"
        "plugdev"
      ];
      initialPassword = "password";
      hashedPasswordFile = config.sops.secrets."${user}_password".path;
    };

    home-manager = {
      users = {
        "${user}" = import ../hosts/${host}/home.nix;
      };
    };
  };
}
