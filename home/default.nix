{
  user,
  config,
  lib,
  isNixos,
  ...
}:
{
  imports = [
    ./cli
    ./options
    ./programs
    ./themes
    ./wayland
  ];

  config = lib.mkMerge [
    {
      home.username = "${user}";
      home.homeDirectory = "/home/${user}";
      xdg = {
        enable = true;
        userDirs = {
          enable = true;
          createDirectories = true;
          desktop = "${config.home.homeDirectory}/desktop";
          documents = "${config.home.homeDirectory}/dox";
          download = "${config.home.homeDirectory}/dl";
          music = "${config.home.homeDirectory}/music";
          pictures = "${config.home.homeDirectory}/pix";
          publicShare = "${config.home.homeDirectory}/pub";
          templates = "${config.home.homeDirectory}/tmpl";
          videos = "${config.home.homeDirectory}/vid";
        };
      };
    }

    (lib.mkIf (isNixos == false) {
      # let home-manager manage itself if not on nixos
      programs.home-manager.enable = true;
    })
  ];
}
