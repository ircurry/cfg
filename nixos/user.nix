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
        "adbusers"
        "kvm"
        "networkmanager"
        "plugdev"
        "wheel"
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
