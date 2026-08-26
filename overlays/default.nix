{
  inputs,
  pkgs,
  lib,
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
      (_: prev: { dfh = inputs.dfh.packages.${pkgs.stdenv.hostPlatform.system}.dfh; })
      # (_: prev: {
      #   yt-dlp = inputs.nixpkgs-yt-dlp.legacyPackages.${pkgs.stdenv.hostPlatform.system}.yt-dlp;
      # })
      (_: prev: {
        freetube = inputs.nixpkgs-freetube.legacyPackages.${pkgs.stdenv.hostPlatform.system}.freetube;
      })
      # (_: prev: {
      #   freetube = prev.freetube.overrideAttrs (
      #     _:
      #     let
      #       inherit (prev) fetchFromGitHub;
      #       hash = "sha256-CQiwAoOJoAZpcDIwqcOfUAvJHLWTdj8fIInlR3qyjg8=";
      #       # hash = lib.fakeHash;
      #     in
      #     rec {
      #       version = "0.25.1";
      #       src = fetchFromGitHub {
      #         owner = "FreeTubeApp";
      #         repo = "FreeTube";
      #         tag = "v${version}-beta";
      #         inherit hash;
      #       };
      #     }
      #   );
      # })
      # (_: prev: {
      #   mullvad-vpn = prev.mullvad-vpn.overrideAttrs (
      #     _:
      #     let
      #       inherit (prev) fetchurl;
      #       selectSystem =
      #         attrs:
      #         attrs.${prev.stdenv.hostPlatform.system}
      #           or (throw "Unsupported system: ${prev.stdenv.hostPlatform.system}");
      #       platform = selectSystem {
      #         x86_64-linux = "amd64";
      #       };
      #       hash = selectSystem {
      #         x86_64-linux = "sha256-OMbuc66AhwaIVgkiooUlttDazGLC5BCTiGPXA46TGso=";
      #         # x86_64-linux = lib.fakeHash;
      #       };
      #     in
      #     rec {
      #       version = "2026.3";
      #       src = fetchurl {
      #         url = "https://github.com/mullvad/mullvadvpn-app/releases/download/${version}/MullvadVPN-${version}_${platform}.deb";
      #         inherit hash;
      #       };
      #     }
      #   );
      # })
      # (_: prev: { nocturne-tools = inputs.nocturne-tools.packages.${pkgs.stdenv.hostPlatform.system}.default; })
    ];
  };
}
