{
  pkgs ? import <nixpkgs> { },
  ags ? pkgs.ags,
  ...
}:
pkgs.mkShell {
  packages = with pkgs; [
    pre-commit
    age
    nh
    just
    sops
    ssh-to-age
    nixfmt-rfc-style
    nixd
    ags
  ];
}
