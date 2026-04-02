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
    nixfmt
    nixd
    # C Stuff
    clang
    gcc
    ccls
    gf
    gdb
  ];
}
