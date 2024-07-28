{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.nocturne.wayland.menu;
  background = config.nocturne.wayland.fuzzel.background + "dd";
  text = config.nocturne.wayland.fuzzel.text + "ff";
  match = config.nocturne.wayland.fuzzel.match + "ff";
  selection = config.nocturne.wayland.fuzzel.selection + "dd";
  selection-text = config.nocturne.wayland.fuzzel.selection-text + "ff";
  selection-match = config.nocturne.wayland.fuzzel.selection-match + "ff";
  border = config.nocturne.wayland.fuzzel.border + "ee";
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
