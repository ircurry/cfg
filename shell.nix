{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  packages = with pkgs; [
    age
    nh
    just
    sops
    ssh-to-age
    nixfmt-rfc-style
    nixd
  ];
}
