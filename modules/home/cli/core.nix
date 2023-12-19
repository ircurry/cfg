{ config, pkgs, inputs, ... }:

{
  
  home.packages = with pkgs; [
    file # file info
    fzf  # cli fuzzy finder
    vim  # cli text editor (I don't want to use nano)
  ];

}
