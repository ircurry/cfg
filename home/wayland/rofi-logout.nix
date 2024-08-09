{
  config,
  lib,
  pkgs,
  ...
}:

let
  menu = config.nocturne.wayland.menu;
  logout = pkgs.writeShellScriptBin "rofi-logout" ''
    op=$( echo -e " Lock\n Logout\n Poweroff\n Reboot\n Suspend" | ${menu.exec-dmenu} ${menu.promptSwitch} "Power Menu" | awk '{print tolower($2)}' )

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
  config = lib.mkIf (menu.name == "rofi") {
    home.packages = [ logout ];
    nocturne.wayland.menu.exec-logout = "${lib.getExe logout}";
  };
}
