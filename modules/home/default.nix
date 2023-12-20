{ config, pkgs, inputs, ... }:

{
  imports = [ 
    ./cli
    ./browsers
    ./editors
    ./graphical
    ./misc
    # ./plasma
    # ./services
    # ./themes
    # ./wayland
  ];

}
