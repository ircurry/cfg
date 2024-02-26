{ user, lib, isNixos, ... }: {
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
    }

    # let home-manager manage itself if not on nixos
    (lib.mkIf (isNixos == false) {
      programs.home-manager.enable = true;
    })
  ];
}
