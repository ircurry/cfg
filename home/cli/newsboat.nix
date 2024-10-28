{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.nocturne.cli.newsboat;
in
{
  config = lib.mkIf (cfg.enable == true) {
    programs.newsboat = {
      enable = true;
      extraConfig = ''
        ##############
        ## NEWSBOAT ##
        ##############
        highlight feedlist "----.*----" red default bold
        highlight feedlist ".*0/0.." default default invis

        bind-key j down
        bind-key k up
        bind-key j next articlelist
        bind-key k prev articlelist
        bind-key J next-feed articlelist
        bind-key K prev-feed articlelist
        bind-key G end
        bind-key g home
        bind-key d pagedown
        bind-key u pageup
        bind-key l open
        bind-key h quit
        bind-key a toggle-article-read
        bind-key n next-unread
        bind-key N prev-unread
        bind-key D pb-download
        bind-key U show-urls
        bind-key x pb-delete

        color listnormal cyan default
        color listfocus black yellow standout bold
        color listnormal_unread blue default
        color listfocus_unread black yellow standout bold
        color info green black bold
        color article cyan default

        macro v set browser "setsid -f mpv '%u' >/dev/null 2>&1" ; open-in-browser-and-mark-read ; set browser "xdg-open '%u'"
        macro d set browser "echo '%u' >> ${config.xdg.userDirs.download}/vids_to_download-$(date +%Y_%U).txt" ; open-in-browser-and-mark-read ; set browser "xdg-open '%u'"
        macro p set browser "echo '%u' >> ${config.xdg.userDirs.download}/pods_to_download-$(date +%Y_%U).txt" ; open-in-browser-and-mark-read ; set browser "xdg-open '%u'"
        macro c set browser "printf '%u' | ${pkgs.wl-clipboard}/bin/wl-copy" ; open-in-browser ; set browser "xdg-open '%u'"

        #############
        ## PODBOAT ##
        #############
        player "mpv --no-video"
      '';
    };
    sops.secrets.links = {
      path = "${config.xdg.configHome}/newsboat/urls";
    };
  };
}
