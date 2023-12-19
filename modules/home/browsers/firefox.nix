{ lib, config, pkgs, inputs, ... }:

let
  cfg = config.nocturne.browsers.firefox;
in
{
  options.nocturne.browsers.firefox = {
    enable = lib.mkEnableOption "Whether to enable firefox.";
  };
  
  config = lib.mkIf cfg.enable {
    programs.firefox = {
      enable = true;
      profiles.recur = {
        isDefault = true;
        bookmarks = [
          {
            name = "wikipedia";
            tags = [ "wiki" ];
            keyword = "wiki";
            url = "https://en.wikipedia.org/";
          }
          {
            name = "archwiki";
            tags = [ "wiki" ];
            keyword = "arch";
            url = "https://wiki.archlinux.org/";
          }
          {
            name = "nixos wiki";
            tags = [ "wiki" "nix" "nixos" ];
            url = "https://nixos.wiki/";
          }
          {
            name = "nixdev";
            tags = [ "wiki" "nix" "nixos" ];
            url = "https://nix.dev/";
          }
        ];
        
        containers = {
          default = {
            color = "blue";
            icon = "circle";
            id = 1;
          };
          unsafe = {
            color = "red";
            icon = "fruit";
            id = 2;
          };
        };

        search = {
          default = "DuckDuckGo";
          force = true;
          engines = {
            "Nix Packages" = {
              urls = [{
                template = "https://search.nixos.org/packages";
                params = [
                  { name = "type"; value = "packages"; }
                  { name = "query"; value = "{searchTerms}"; }
                ];
              }];
              
              icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
              definedAliases = [ "@np" ];
            };
          };
        };

        extensions = with inputs.firefox-addons.packages."x86_64-linux"; [
          ublock-origin
          libredirect
        ];
      };
    };
  };
}
