{ lib, config, pkgs, inputs, ... }:

let
  cfg = config.nocturne.graphical.firefox;
in
{
  imports = [ inputs.arkenfox.hmModules.default ];
  
  config = lib.mkIf cfg.enable {
    
    programs.firefox = {
      enable = true;
      arkenfox = {
        enable = true;
        version = "119.0";
      };
      
      profiles.recur = {
        isDefault = true;
        id = 0;
        arkenfox = {
          enable = true;
          "0000".enable = true;
          "0100".enable = true;
          "0200".enable = true;
          "0300".enable = true;
          "0400".enable = true;
          "0600".enable = true;
          "0700".enable = true;
          "0800" = {
            enable = true;
            "0810"."browser.formfill.enable".value = false;
          };
          "0900".enable = true;
          "1200".enable = true;
          "2600".enable = true;
          "2700".enable = true;
        };
        
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
        
        # containers = {
        #   default = {
        #     color = "blue";
        #     icon = "circle";
        #     id = 1;
        #   };
        #   unsafe = {
        #     color = "red";
        #     icon = "fruit";
        #     id = 2;
        #   };
        # };

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
            "Noogle" = {
              urls = [{
                template = "https://noogle.dev/q";
                params = [
                  { name = "term"; value = "{searchTerms}"; }
                ];
              }];
              
              icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
              definedAliases = [ "@no" ];
            };
            "MyNixOS" = {
              urls = [{
                template = "https://mynixos.com/search";
                params = [
                  { name = "q"; value = "{searchTerms}"; }
                ];
              }];
              
              icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
              definedAliases = [ "@mn" ];
            };
            "Odysee" = {
              urls = [{
                template = "https://odysee.com/$/search";
                params = [
                  { name = "q"; value = "{searchTerms}"; }
                ];
              }];
              
              icon = "${pkgs.libsForQt5.breeze-icons}/share/icons/breeze-dark/actions/16/media-playback-start.svg";
              definedAliases = [ "@od" ];
            };
            "Invidious" = {
              urls = [{
                template = "https://invidious.slipfox.xyz/search";
                params = [
                  { name = "q"; value = "{searchTerms}"; }
                ];
              }];
              
              icon = "${pkgs.libsForQt5.breeze-icons}/share/icons/breeze-dark/actions/16/media-playback-start.svg";
              definedAliases = [ "@iv" ];
            };
          };
        };

        extensions = with inputs.firefox-addons.packages."x86_64-linux"; [
          ublock-origin
          libredirect
          multi-account-containers
        ];
      };
      
      profiles.school = {
        isDefault = false;
        id = 1;
        arkenfox = {
          enable = true;
          "0000".enable = true;
          "0100".enable = true;
          "0200".enable = true;
          "0300".enable = true;
          "0400".enable = true;
          "0600".enable = true;
          "0700".enable = true;
          "0800" = {
            enable = true;
            "0810"."browser.formfill.enable".value = false;
          };
          "0900".enable = true;
          # "1200".enable = true;
          "2600".enable = true;
          "2700".enable = true;
        };
        
        bookmarks = [
          {
            name = "wikipedia";
            tags = [ "wiki" ];
            keyword = "wiki";
            url = "https://en.wikipedia.org/";
          }
        ];
        
        # containers = {
        #   default = {
        #     color = "blue";
        #     icon = "circle";
        #     id = 1;
        #   };
        #   unsafe = {
        #     color = "red";
        #     icon = "fruit";
        #     id = 2;
        #   };
        # };

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
          multi-account-containers
          user-agent-string-switcher
        ];
      
      };
      profiles.games = {
        isDefault = false;
        id = 2;
        
        # containers = {
        #   temp = {
        #     color = "blue";
        #     icon = "circle";
        #     id = 1;
        #   };
        # };

        search = {
          default = "DuckDuckGo";
          force = true;
        };

        extensions = with inputs.firefox-addons.packages."x86_64-linux"; [
          multi-account-containers
          ublock-origin
        ];
      
      };
    };
  };
}
