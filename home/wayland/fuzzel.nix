{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.nocturne.wayland.menu;
  background = config.nocturne.themes.colors.base00 + "dd";
  text = config.nocturne.themes.colors.base05 + "ff";
  match = config.nocturne.themes.colors.base08 + "ff";
  selection = config.nocturne.themes.colors.base01 + "dd";
  selection-text = text;
  selection-match = match;
  border = config.nocturne.themes.colors.base0D + "ee";
  logout = pkgs.writeShellScriptBin "rofi-logout" ''
    op=$( echo -e " Lock\n Logout\n Poweroff\n Reboot\n Suspend" | ${cfg.exec-dmenu} ${cfg.promptSwitch} "Power Menu: " | awk '{print tolower($2)}' )

    case "$op" in 
            poweroff) systemctl poweroff ;;
            reboot) systemctl reboot ;;
            suspend) systemctl suspend && ${config.nocturne.wayland.lock.exec} ;;
            lock) ${config.nocturne.wayland.lock.exec} ;;
            logout) loginctl terminate-user $USER ;;
    esac
  '';

in
{
  config = lib.mkMerge [
    (lib.mkIf (cfg.name == "fuzzel") {
      programs.fuzzel = {
        enable = true;
        settings = {
          main = {
            font = "JetBrainsMono Nerd Font:size=12";
            fields = "name,filename,generic,keywords";
            terminal = "${config.nocturne.wayland.terminal.exec-center}";

          };
          border = {
            width = 2;
            radius = 10;
          };
          colors = {
            inherit
              background
              text
              match
              selection
              selection-text
              selection-match
              border
              ;
          };
        };
      };
      nocturne.wayland.menu = {
        promptSwitch = "-p";
        exec = "${lib.getExe pkgs.fuzzel}";
        exec-run = "${lib.getExe pkgs.custom.fuzzel-run}";
        exec-dmenu = "${lib.getExe pkgs.fuzzel} --dmenu";
        exec-logout = "${lib.getExe logout}";
      };
    })
  ];
}
