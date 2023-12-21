{ config, lib, pkgs, ... }:

{

  options.nocturne.cli.scripts = {
    youtube-scripts = lib.mkEnableOption "Whether to enable youtube scripts";
  };
  
  imports = [
    ./youtube-scripts.nix
  ];
}
