{ inputs, pkgs, ... }:
{
  config = {
    nixpkgs.overlays = [
      inputs.emacs-overlay.overlay
      # custom packages
      (_: prev: {
        custom =
          (prev.custom or { })
          // (import ../packages {
            inherit (prev) pkgs;
            inherit inputs;
          });
      })
      (_: prev: {
        ytfzf = (
          prev.ytfzf.override {
            ueberzugpp = inputs.nixpkgs-ueberzugpp.legacyPackages.${pkgs.system}.ueberzugpp;
          }
        );
      })
      (_: prev: { dfh = inputs.dfh.packages.${pkgs.system}.dfh; })
      # (_: prev: { nocturne-tools = inputs.nocturne-tools.packages.${pkgs.system}.default; })
    ];
  };
}
