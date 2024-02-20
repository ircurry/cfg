{
  description = "Nixos config flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    firefox-addons = {
      url = "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
    arkenfox = {
      url = "github:dwarfmaster/arkenfox-nixos";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ## Bleeding edge Hyprland
    # hyprland.url = "github:hyprwm/Hyprland";

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    devenv.url = "github:cachix/devenv";

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      forAllSystems = function:
        nixpkgs.lib.genAttrs ["x86_64-linux"] (system: function nixpkgs.legacyPackages.${system});
    in
    {
    
      nixosConfigurations.default = nixpkgs.lib.nixosSystem {
        specialArgs = {inherit inputs;};
        modules = [ 
          ./hosts/default/configuration.nix
          inputs.home-manager.nixosModules.default
          inputs.sops-nix.nixosModules.sops
        ];
      };
      
      nixosConfigurations."chopin" = nixpkgs.lib.nixosSystem {
        specialArgs = {inherit inputs;};
        modules = [ 
          ./hosts/chopin/configuration.nix
          inputs.home-manager.nixosModules.default
          inputs.sops-nix.nixosModules.sops
        ];
      };

      nixpkgs.overlays = [ (import self.inputs.emacs-overlay) ];

      devShells = forAllSystems (pkgs: {
        default = inputs.devenv.lib.mkShell {
          inherit inputs pkgs;
          modules = [
            ({pkgs, ...}: {
              packages = [
                pkgs.age
                pkgs.nh
                pkgs.sops
                pkgs.ssh-to-age
              ];
              enterShell = ''
                eval "$(ssh-agent -s)"
                ssh-add ~/.ssh/id_github
              '';
              languages.nix.enable = true;
              scripts = {
                fl.exec = ''
                  nix-diff() {
                      nix store diff-closures "$(${pkgs.fd}/bin/fd 'system-' /nix/var/nix/profiles/ -j 1 | sort --reverse | ${ pkgs.fzf }/bin/fzf )" /nix/var/nix/profiles/system | column -t -s ':' -o ' (' 
                  }
                  inputs() {
                      inputs=$(nix flake metadata --json \
                               | ${pkgs.jq}/bin/jq ".locks.nodes.root.inputs | keys[]" \
                               | sed "s/\"//g"
                            )

                      select=$(printf "all\n$inputs" | ${pkgs.fzf}/bin/fzf)

                      if [ -z $select ]; then
                          exit 0
                      fi

                      case "$select" in
                          all) nix flake update ;;
                          *) nix flake lock --update-input $select ;;
                      esac
                  }
                  rebuild() {
                      select=$(printf "switch\ntest\nboot\n" | ${pkgs.fzf}/bin/fzf)

                      if [ -z $select ]; then
                          exit 0
                      fi

                      nh os $select --nom .
                  }
                  case $1 in
                      diff) nix-diff ;;
                      in|inputs) inputs ;;
                      rb|rebuild) rebuild ;;
                      up|update) nix flake update && rebuild;;
                      *) echo -e "\033[1mUnknown command, please try again.\033[0m";;
                  esac
                '';
              };
            })
          ];
        };
      });
      
    };
}
