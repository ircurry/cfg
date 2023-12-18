{ config, pkgs, inputs, ... }:

{
  imports = [ 
    ./cli
    # ./browsers
    ./editors
    # ./graphical
    # ./plasma
    # ./services
    # ./shells
    # ./themes
    # ./wayland
  ];

  config = {
    editors.emacs.enable = true;

  };
}
