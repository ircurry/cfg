{
  lib,
  config,
  pkgs,
  inputs,
  ...
}:

let
  cfg = config.nocturne.graphical.firefox;
  way-cfg = config.nocturne.wayland.browser;
in
{
  imports = [ inputs.arkenfox.hmModules.default ];

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      programs.firefox = {
        enable = true;
        arkenfox = {
          enable = true;
          # TODO: update this
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

          bookmarks = {
            force = true;
            settings = [
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
                tags = [
                  "wiki"
                  "nix"
                  "nixos"
                ];
                url = "https://nixos.wiki/";
              }
              {
                name = "nixdev";
                tags = [
                  "wiki"
                  "nix"
                  "nixos"
                ];
                url = "https://nix.dev/";
              }
              {
                name = "consumer rights wiki";
                tags = [ "wiki" ];
                keyword = "consumer";
                url = "https://consumerrights.wiki/";
              }
            ];
          };

          search = {
            default = "ddg";
            force = true;
            engines = {
              "Nix Packages" = {
                urls = [
                  {
                    template = "https://search.nixos.org/packages";
                    params = [
                      {
                        name = "type";
                        value = "packages";
                      }
                      {
                        name = "query";
                        value = "{searchTerms}";
                      }
                    ];
                  }
                ];

                icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                definedAliases = [ "@np" ];
              };
              "Noogle" = {
                urls = [
                  {
                    template = "https://noogle.dev/q";
                    params = [
                      {
                        name = "term";
                        value = "{searchTerms}";
                      }
                    ];
                  }
                ];

                icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                definedAliases = [ "@no" ];
              };
              "MyNixOS" = {
                urls = [
                  {
                    template = "https://mynixos.com/search";
                    params = [
                      {
                        name = "q";
                        value = "{searchTerms}";
                      }
                    ];
                  }
                ];

                icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                definedAliases = [ "@mn" ];
              };
              "Odysee" = {
                urls = [
                  {
                    template = "https://odysee.com/$/search";
                    params = [
                      {
                        name = "q";
                        value = "{searchTerms}";
                      }
                    ];
                  }
                ];

                icon = "${pkgs.libsForQt5.breeze-icons}/share/icons/breeze-dark/actions/16/media-playback-start.svg";
                definedAliases = [ "@od" ];
              };
              "youtube" = {
                urls = [
                  {
                    template = "https://www.youtube.com/results";
                    params = [
                      {
                        name = "search_query";
                        value = "{searchTerms}";
                      }
                    ];
                  }
                ];

                icon = "${pkgs.libsForQt5.breeze-icons}/share/icons/breeze-dark/actions/16/media-playback-start.svg";
                definedAliases = [
                  "@yt"
                ];
              };
            };
          };

          extensions.packages = with inputs.firefox-addons.packages."${pkgs.system}"; [
            darkreader
            ublock-origin
            libredirect
            multi-account-containers
            tree-style-tab
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

          bookmarks = {
            force = true;
            settings = [
              {
                name = "wikipedia";
                tags = [ "wiki" ];
                keyword = "wiki";
                url = "https://en.wikipedia.org/";
              }
            ];
          };

          search = {
            default = "ddg";
            force = true;
            engines = {
              "Nix Packages" = {
                urls = [
                  {
                    template = "https://search.nixos.org/packages";
                    params = [
                      {
                        name = "type";
                        value = "packages";
                      }
                      {
                        name = "query";
                        value = "{searchTerms}";
                      }
                    ];
                  }
                ];

                icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                definedAliases = [ "@np" ];
              };
            };
          };

          extensions.packages = with inputs.firefox-addons.packages."${pkgs.system}"; [
            ublock-origin
            libredirect
            multi-account-containers
            user-agent-string-switcher
            tree-style-tab
          ];
        };
        profiles.games = {
          isDefault = false;
          id = 2;

          search = {
            default = "ddg";
            force = true;
          };

          extensions.packages = with inputs.firefox-addons.packages."${pkgs.system}"; [
            multi-account-containers
            ublock-origin
          ];
        };
        profiles.amazon = {
          isDefault = false;
          id = 3;

          search = {
            default = "ddg";
            force = true;
          };

          extensions.packages = with inputs.firefox-addons.packages."${pkgs.system}"; [
            multi-account-containers
            ublock-origin
          ];
        };
        profiles.banking = {
          isDefault = false;
          id = 4;

          search = {
            default = "ddg";
            force = true;
          };

          extensions.packages = with inputs.firefox-addons.packages."${pkgs.system}"; [
            multi-account-containers
            ublock-origin
          ];
        };
        profiles.spotify = {
          isDefault = false;
          id = 5;

          search = {
            default = "ddg";
            force = true;
          };

          extensions.packages = with inputs.firefox-addons.packages."${pkgs.system}"; [
            multi-account-containers
            ublock-origin
          ];
        };
        profiles.zoom = {
          isDefault = false;
          id = 6;

          search = {
            default = "ddg";
            force = true;
          };

          extensions.packages = with inputs.firefox-addons.packages."${pkgs.system}"; [
            multi-account-containers
            ublock-origin
          ];
        };
      };
      nocturne.wayland.startup = [
        {
          name = "firefox";
          exec = "firefox";
          packages = [ config.programs.firefox.package ];
          workspace = 4;
        }
      ];
    })
    (lib.mkIf (way-cfg.name == "firefox") {
      home.sessionVariables =
        let
          firefox-exe = lib.getExe pkgs.firefox;
        in
        {
          DEFAULT_BROWSER = firefox-exe;
          BROWSER = firefox-exe;
        };
      xdg.mimeApps.defaultApplications = {
        "x-scheme-handler/http" = "firefox.desktop";
        "x-scheme-handler/https" = "firefox.desktop";
      };
    })
  ];
}
