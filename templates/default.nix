let
  c = {
    path = ./c;
    description = "C flake and dev enviornment";
  };
  generic = {
    path = ./generic;
    description = "Generic Programming Template";
  };
  haskell = {
    path = ./haskell;
    description = "Haskell flake and dev enviornment";
  };
  zig = {
    path = ./zig;
    description = "Zig flake and dev enviornment";
  };
in
{
  inherit
    c
    generic
    haskell
    zig
    ;
  gen = generic;
  hs = haskell;
}
