let
  haskell = {
    path = ./haskell;
    description = "Haskell flake and dev enviornment";
  };
  c = {
    path = ./c;
    description = "C flake and dev enviornment";
  };
in
{
  inherit haskell c;
  hs = haskell;
}
