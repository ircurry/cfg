{ pkgs, inputs,... }: let
  inherit (pkgs) lib callPackage;
in {
  id3 = callPackage ./id3 { };
  youtubeScripts = callPackage ./youtubeScripts { };
}
