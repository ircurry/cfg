{ lib, nixpkgs }:

rec {
  # foldl but the accumulator is the first element in the list
  foldl1 =
    f: list:
    let
      start = builtins.head list;
      list' = builtins.tail list;
    in
    lib.lists.foldl f start list';

  # foldr but the accumulator is the first element in the list
  foldr1 =
    f: list:
    let
      start = lib.lists.last list;
      list' = lib.lists.init list;
    in
    lib.lists.foldr f start list';

  # recieve string and check if it is empty or contains only white space
  isEmptyStr = str: if (builtins.match "^[[:space:]]*$" str) != null then true else false;

  startsWithHash = str: if (builtins.match "^#.*" str) != null then true else false;

  # take a predicate function and a list of strings and return a list of all strings matching predicate
  filterStrList =
    predicate: strList: lib.foldl (acc: x: if predicate x then acc ++ [ x ] else acc) [ ] strList;

  # take a list of string and return that list without empty or white space only strings
  removeEmpty = strList: filterStrList (str: !isEmptyStr str) strList;

  # Take an attr set of strings, attrsets usable by writeShellApplication, or a
  # derivation and return an attr of derivations. This could then be used by
  # lib.attrValues to produce a list of packages. Shamelessly stolen from
  # iynaix's configs:
  # https://github.com/iynaix/dotfiles/blob/c3bf4579f465300a3b7eed969ec4c27b3b1a166b/lib.nix#L26
  mkShellPackages =
    pkgs: shellPkgs:
    lib.mapAttrs (
      name: value:
      if lib.isString value then
        pkgs.writeShellApplication {
          inherit name;
          text = value;
        }
      else if lib.isDerivation value then
        value
      else
        pkgs.writeShellApplication (value // { inherit name; })
    ) shellPkgs;

  hyprlandMonitorsToString =
    monitors:
    let
      nameToString = name: if name == null then "" else "${name}";
      resolutionToString =
        res:
        if res == null then
          "prefered"
        else
          "${toString res.width}x${toString res.height}@${toString res.refresh_rate}";
      positionToString = pos: if pos == null then "auto" else "${toString pos.x}x${toString pos.y}";
      scaleToString = scale: if scale == null then "auto" else "${toString scale}";
      mapFn =
        monitor:
        if monitor.enabled then
          "${nameToString monitor.name},${resolutionToString monitor.resolution},${positionToString monitor.position},${scaleToString monitor.scale}"
        else
          "${nameToString monitor.name},disabled";
    in
    map mapFn monitors;

  hyprlandDefaultProfile =
    defaultProfile: monitorProfiles:
    let
      profilesWithDefaultName = builtins.filter (x: x != { }) (
        lib.map (x: if defaultProfile == x.name then x else { }) monitorProfiles
      );
      profile =
        assert (builtins.length profilesWithDefaultName) == 1;
        builtins.head profilesWithDefaultName;
    in
    hyprlandMonitorsToString profile.monitors;
}
