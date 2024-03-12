{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.nocturne.wayland.logout.name;
  menu = config.nocturne.wayland.menu.name;
  rofi-logout = pkgs.writeShellScriptBin "rofi-logout" ''
    op=$( echo -e " Lock\n Logout\n Poweroff\n Reboot\n Suspend" | ${pkgs.rofi-wayland}/bin/rofi -i -p "Power Menu" -dmenu | awk '{print tolower($2)}' )

    case $op in 
            poweroff) systemctl poweroff ;;
            reboot) systemctl reboot ;;
            suspend) systemctl suspend && ${config.nocturne.wayland.lock.exec} ;;
            lock) ${config.nocturne.wayland.lock.exec} ;;
            logout) loginctl terminate-user $USER ;;
    esac
  '';
in
{
  config = lib.mkIf (cfg == "rofi-logout") {
    home.packages = [
      pkgs.nerdfonts
      rofi-logout
    ];
    nocturne.wayland.logout.exec = "${lib.getExe rofi-logout}";
    warnings = lib.optionals (menu != "rofi-wayland") [
      "rofi-logout is enabled but ${menu} is enabled instead of rofi-wayland"
    ];
    assertions = [
      {
        assertion = (menu == "rofi-wayland") || (config.nocturne.graphical.rofi.enable);
        message = "rofi-logout is the logout menu but rofi is not installed. Please enable rofi or set it as the default menu.";
      }
    ];
  };
}
