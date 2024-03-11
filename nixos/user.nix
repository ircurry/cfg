{ user, config, host, ... }:
{
  config = {
    users.users."${user}" = {
      isNormalUser = true;
      description = "Ian Curran";
      extraGroups = [ "networkmanager" "wheel" ];
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
