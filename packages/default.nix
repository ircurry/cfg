{ pkgs, inputs,... }: let
  inherit (pkgs) lib callPackage;
in {
  youtubeScripts = callPackage ./youtubeScripts { };
}
