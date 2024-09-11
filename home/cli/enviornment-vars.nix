{
  config,
  lib,
  pkgs,
  ...
}:

let
  c = config.xdg.configHome;
  d = config.xdg.dataHome;
  cache = config.xdg.cacheHome;
  home = config.home.homeDirectory;
  term-cfg = config.nocturne.wayland.terminal;
in
{
  config = {
    home.sessionVariables = {
      NIXOS_OZONE_WL = "1";
      NOCTURNE_CONF_DIR = home + "/cfg";
      TMUX_TMPDIR = "$XDG_RUNTIME_DIR";
      GNUPGHOME = d + "/gnupg";
      LESSHISTFILE = "/dev/null";
      GOPATH = d + "/go/";
      TERMINAL = "${term-cfg.exec}";
      FZF_DEFAULT_OPTS = "--color='prompt:3,pointer:3,bg+:0,fg+:6,hl:2,hl+:3:bold,header:3' --reverse --border --prompt='# ' --bind=alt-1:first,alt-2:last";
      #MY_SESSION_VAR = "works";
    };
  };
}
