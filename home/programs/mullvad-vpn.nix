{
  lib,
  config,
  pkgs,
  ...
}:

let
  cfg = config.nocturne.graphical.mullvad-vpn;
  cfg-browser = config.nocturne.graphical.mullvadBrowser;
in
{
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [ mullvad-vpn ];
    warnings =
      lib.optionals ((cfg.enable == true) && (cfg-browser.enable == false)) [
        "Mullvad VPN client is enabled but Mullvad Browser is not"
      ]
      ++ lib.optionals ((cfg.enable == false) && (cfg-browser.enable == true)) [
        "Mullvad Browser is enabled but Mullvad VPN client is not"
      ];
  };
}
