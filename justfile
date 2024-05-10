selrb := 'switch'
selin := 'nixpkgs'

test:
	nh os test .

rb:
	nh os {{ selrb }} .

update:
	nix flake update

input:
	nix flake lock --update-input {{ selin }}

gc:
	sudo nix-collect-garbage --delete-older-than 7d

gc-old:
	sudo nix-collect-garbage --delete-old
