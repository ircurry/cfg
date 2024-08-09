{ pkgs, ... }:
let
  inherit (pkgs) callPackage;
in
{
  fuzzel-run = callPackage ./fuzzel-run { };
  id3 = callPackage ./id3 { };
  youtubeScripts = callPackage ./youtubeScripts { };
}
