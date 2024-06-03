let
  c = {
    path = ./c;
    description = "C flake and dev enviornment";
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
  inherit c haskell zig;
  hs = haskell;
}
