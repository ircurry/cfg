{
  description = "Generic Programming Template";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs =
    { nixpkgs, ... }@inputs:
    let
      # ===Nixpkgs Lib===
      inherit (nixpkgs) lib;

      # ===Dealing with System===
      forSystem = function: system: function nixpkgs.legacyPackages.${system};

      systems = [ "x86_64-linux" ];

      forAllSystems = function: lib.genAttrs systems (forSystem function);
    in
    {
      packages = forAllSystems (pkgs: rec {
        exapmple = pkgs.writeShellApplication {
          name = "package-example";
          # runtimeInputs = [ ];
          text = lib.readFile ''
            echo "Hello, World!"
          '';
          meta = {
            description = "Test Package";
            licence = lib.licences.mit;
            platforms = lib.platforms.linux;
          };
        };
      });

      # ===Development Enviornment===
      devShells = forAllSystems (pkgs: {
        default = import ./shell.nix {
          inherit pkgs;
          ags = inputs.ags.packages.${pkgs.stdenv.hostPlatform.system}.agsFull;
        };
      });
    };
}
