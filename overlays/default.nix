{
  inputs,
  sys,
  lib,
  ...
}:
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
        # fix waybar wireplumber issue
        waybar =
          assert (lib.assertMsg (prev.waybar.version == "0.10.0") "is the fix still necessary?");
          inputs.nixpkgs-fix-waybar.legacyPackages.${sys}.waybar;
      })
    ];
  };
}
