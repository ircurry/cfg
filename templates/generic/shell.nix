{
  pkgs ? import <nixpkgs> { },
  ...
}:
pkgs.mkShell {
  packages = with pkgs; [
    # General Stuff
    pre-commit
    just
    # Nix Stuff
    nixfmt-rfc-style
    nixd
    # C Stuff
    clang
    gcc
    ccls
    gf
    gbd
  ];
}
