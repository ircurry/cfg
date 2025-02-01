{ config, lib, ... }:
let
  cfg = config.nocturne.network.mullvad-vpn;
  cfg-hm = config.nocturne.hm.programs.mullvad-vpn;
in
{
  options =
    let
      inherit (lib) mkEnableOption;
    in
    {
      nocturne.network.mullvad-vpn.enable = mkEnableOption "Enable Mullvad VPN";
    };

  config = lib.mkIf ((cfg.enable == true) || (cfg-hm.enable == true)) {
    services.mullvad-vpn.enable = true;
  };
}
