{
  host ? "chopin", # default host is my framework laptop
  user ? "recur", # default user is my username on chopin
  ...
}:
let
  flake = builtins.getFlake (builtins.toString ./.);
in
rec {
  # ===Basic Values===
  inherit flake host user; # raw access to the flake, user, and host
  inherit (flake) inputs packages; # flake inputs and custom packages
  inherit (flake.inputs) nixpkgs; # nixpkgs
  inherit (flake.inputs.nixpkgs) lib; # nixpkgs
  mylib = flake.lib; # custom lib

  # ===Current Host Stuff===
  c = flake.nixosConfigurations.${host}.config; # nixos config
  inherit (flake.nixosConfigurations.${host}) config;
  inherit (flake.nixosConfigurations.${host}.config) hm; # home manager config
  n = flake.nixosConfigurations.${host}.config.hm.nocturne; # custom hm options
  inherit (flake.nixosConfigurations.${host}.config.hm) nocturne;
  nsys = flake.nixosConfigurations.${host}.config.noctsys; # custom nixos system options
  inherit (flake.nixosConfigurations.${host}.config) noctsys;
  inherit (flake.nixosConfigurations.${host}) pkgs; # packages used by nixos and hm
  emacspkg = hm.programs.emacs.package;

  # ===Chopin===
  chopin = flake.nixosConfigurations.chopin.config;
  chopino = chopin.noctsys;
  chopinhm = chopin.hm;
  chopinhmo = chopin.hm.nocturne;
}
