{ inputs, system, ... }:

inputs.devenv.lib.mkShell {
  inherit inputs;

  pkgs = import inputs.nixpkgs {
    inherit system;
    overlays = [ (_: prev: { nixfmt = prev.nixfmt-rfc-style; }) ];
  };

  modules = [
    (
      { pkgs, ... }:
      {
        packages = [
          pkgs.age
          inputs.nh.packages.${system}.default
          pkgs.sops
          pkgs.ssh-to-age
          pkgs.nil
          #pkgs.nixfmt-rfc-style
        ];

        # pre-commit = {
        #   hooks.nixfmt = {
        #     enable = true;
        #   };
        # };

        languages.nix.enable = true;
        scripts = {
          fl.exec = ''
            nix-diff() {
                nix store diff-closures "$(${pkgs.fd}/bin/fd 'system-' /nix/var/nix/profiles/ -j 1 | sort --reverse | ${pkgs.fzf}/bin/fzf )" /nix/var/nix/profiles/system | column -t -s ':' -o ' (' 
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

                nh os $select .
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
      }
    )
  ];
}
