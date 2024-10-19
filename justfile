nixos-config := 'chopin'
rb-action := 'switch'
# Eat, compile, (ansi)term, and comint programs do not work with nom unfortunately
nom := if env_var_or_default('INSIDE_EMACS', "") =~ ".*,(eat|comint|compile|term)(:.*)?" { "--no-nom" } else { "" }

test:
	nh os test . {{ nom }}

rb:
	nh os {{ rb-action }} . {{ nom }}

update:
	nix flake update

input input:
	nix flake lock --update-input {{ input }}

gc:
	sudo nix-collect-garbage --delete-older-than 7d
	nix-collect-garbage --delete-older-than 7d

gc-old:
	sudo nix-collect-garbage --delete-old

vm:
	nixos-rebuild build-vm --flake .'#'{{ nixos-config }} && ./result/bin/run-{{ nixos-config }}-vm
	
