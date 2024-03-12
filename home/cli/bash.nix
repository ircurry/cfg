{
  config,
  pkgs,
  lib,
  ...
}:

{
  config = {
    programs.bash = {
      enable = true;
      initExtra = ''
        eval "$(direnv hook bash)"
      '';
      profileExtra = config.nocturne.wayland.compositor.profileExtra;
    };
  };
}
