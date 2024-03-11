{ pkgs, ... }:

{
  config = { home.packages = with pkgs; [ imagemagick ]; };
}
