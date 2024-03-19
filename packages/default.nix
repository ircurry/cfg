{ pkgs, ... }:
let
  inherit (pkgs) callPackage;
in
{
  id3 = callPackage ./id3 { };
  youtubeScripts = callPackage ./youtubeScripts { };
}
