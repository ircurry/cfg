{ pkgs, ... }: {
  config = {
    home.shellAliases = {
      cl = "clear";
      info = "info --vi-keys";
      icd = "__zoxide_zi";

      diff = "diff --color=auto";
      grep = "grep --color=auto";

      rm = "rm -i";
      ls = "${pkgs.eza}/bin/eza -h --group-directories-first --icons";
      ll =
        "${pkgs.eza}/bin/eza -h --group-directories-first --icons -L 1 -T -l";
      la =
        "${pkgs.eza}/bin/eza -h --group-directories-first --icons -L 1 -T -l -a";
      lt =
        "${pkgs.eza}/bin/eza -h --group-directories-first --icons -L 4 -T -l";
      mv = "mv -i";
      cp = "cp -i";
      tp = "${pkgs.trashy}/bin/trash put";
      zo = "zoxide";

      ip = "ip -color=auto";
      wget = "${pkgs.wget}/bin/wget --no-hsts";

      am = "${pkgs.amfora}/bin/amfora";
      irssi =
        "${pkgs.irssi}/bin/irssi --config=$XDG_CONFIG_HOME/irssi/config --home=$XDG_DATA_HOME/irssi";
    };
  };
}
