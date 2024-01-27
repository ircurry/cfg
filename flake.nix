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
        ];
      };
      
      nixosConfigurations."chopin" = nixpkgs.lib.nixosSystem {
        specialArgs = {inherit inputs;};
        modules = [ 
          ./hosts/chopin/configuration.nix
          inputs.home-manager.nixosModules.default
        ];
      };

      nixpkgs.overlays = [ (import self.inputs.emacs-overlay) ];

      devShells = forAllSystems (pkgs: {
        default = inputs.devenv.lib.mkShell {
          inherit inputs pkgs;
          modules = [
            ({pkgs, ...}: {
              languages.nix.enable = true;
              scripts = {
                nix-diff.exec = ''nix store diff-closures "$(${pkgs.fd}/bin/fd 'system-' /nix/var/nix/profiles/ -j 1 | sort --reverse | ${ pkgs.fzf }/bin/fzf )" /nix/var/nix/profiles/system | column -t -s ':' -o ' (' '';
                flk-inputs.exec = ''
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
                '';
                flk-rebuild.exec = ''
                  select=$(printf "switch\ntest\nboot\nbuild\n" | ${pkgs.fzf}/bin/fzf)

                  if [ -z $select ]; then
                      exit 0
                  fi

                  sudo nixos-rebuild $select --flake .
                '';
              };
            })
          ];
        };
      });
      
    };
}
