{ inputs, lib, ... }:
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
    ];
  };
}
