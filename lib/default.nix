{ lib
, nixpkgs
}:

rec {
  # foldl but the accumulator is the first element in the list
  foldl1 = f: list: let
    start = builtins.head list;
    list' = builtins.tail list;
  in lib.lists.foldl f start list';

  # foldr but the accumulator is the first element in the list
  foldr1 = f: list: let
    start = lib.lists.last list;
    list' = lib.lists.init list;
  in lib.lists.foldr f start list';
}
