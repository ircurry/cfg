{ config, pkgs, inputs, ... }:

{
  programs.git = {
    enable = true;
    userEmail = "icurran@protonmail.com";
    userName = "Ian Curran";
  };

}
