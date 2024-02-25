{ inputs, user, ... }:
{
  config = {
    home-manager = {
      sharedModules = [
        inputs.sops-nix.homeManagerModules.sops
      ];
    };

    sops = {
      defaultSopsFile = ../secrets/secrets.yaml;
      defaultSopsFormat = "yaml";
      age.keyFile = "/home/${user}/.config/sops/age/keys.txt";
      secrets = {
        "${user}_password".neededForUsers = true;
      };
    };
  };
}
