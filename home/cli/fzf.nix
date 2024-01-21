{ pkgs, ... }: {
  config = {
    home.packages = [
      pkgs.fzf
    ];
    home.sessionVariables = {
      FZF_DEFAULT_OPTS = "--color='prompt:3,pointer:3,bg+:0,fg+:6,hl:2,hl+:3:bold,header:3' --reverse --border --prompt='# ' --bind=alt-1:first,alt-2:last";
    };
  };
}
