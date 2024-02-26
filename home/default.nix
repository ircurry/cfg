{ user, ... }: {
  imports = [
    ./cli
    ./options
    ./programs
    ./themes
    ./wayland
  ];

  config = {
    home.username = "${user}";
    home.homeDirectory = "/home/${user}";
  };
}
