{ inputs, ... }:
{
  config = {
    nixpkgs.overlays = [ inputs.emacs-overlay.overlay ];
  };
}
