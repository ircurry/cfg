{ inputs, pkgs, ... }:
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
      # (_: prev: { nocturne-tools = inputs.nocturne-tools.packages.${pkgs.system}.default; })
    ];
  };
}
