nixos-config := 'chopin'
rb-action := 'switch'
input := 'nixpkgs'

test:
	nh os test .

rb:
	nh os {{ rb-action }} .

update:
	nix flake update

input:
	nix flake lock --update-input {{ input }}

gc:
	sudo nix-collect-garbage --delete-older-than 7d

gc-old:
	sudo nix-collect-garbage --delete-old

vm:
	nixos-rebuild build-vm --flake .'#'{{ nixos-config }} && ./result/bin/run-{{ nixos-config }}-vm
	
