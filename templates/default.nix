let
  haskell = {
    path = ./haskell;
    description = "Haskell flake and dev enviornment";
  };
in
{
  inherit haskell;
  hs = haskell;
}
