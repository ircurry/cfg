{
  inputs,
  pkgs,
  ...
}:
{
  config = {
    nixpkgs.overlays = [
      inputs.emacs-overlay.overlay
      inputs.niri.overlays.niri
      # custom packages
      (_: prev: {
        custom =
          (prev.custom or { })
          // (import ../packages {
            inherit (prev) pkgs;
            inherit inputs;
          });
      })
      (_: prev: { dfh = inputs.dfh.packages.${pkgs.system}.dfh; })
      (_: prev: { yt-dlp = inputs.nixpkgs-yt-dlp.legacyPackages.${pkgs.system}.yt-dlp; })
      (_: prev: {
        mullvad-vpn = prev.mullvad-vpn.overrideAttrs (
          _:
          let
            inherit (prev) fetchurl;
            selectSystem =
              attrs:
              attrs.${prev.stdenv.hostPlatform.system}
                or (throw "Unsupported system: ${prev.stdenv.hostPlatform.system}");
            platform = selectSystem {
              x86_64-linux = "amd64";
            };
            hash = selectSystem {
              x86_64-linux = "sha256-ao/NuZB4b6eS+/+Di+2SdymC/wEVYoqFwQ9iXx4n0Pg=";
            };
          in
          rec {
            version = "2025.9";
            src = fetchurl {
              url = "https://github.com/mullvad/mullvadvpn-app/releases/download/${version}/MullvadVPN-${version}_${platform}.deb";
              inherit hash;
            };
          }
        );
      })
      # (_: prev: { nocturne-tools = inputs.nocturne-tools.packages.${pkgs.system}.default; })
    ];
  };
}
