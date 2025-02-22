{
  fetchFromGitHub,
  fakeHash,
  trivialBuild,
}:
trivialBuild rec {
  pname = "popper";
  version = "c0dced3";
  src = fetchFromGitHub {
    owner = "karthink";
    repo = "popper";
    rev = "c0dced3cdff15d3ebfc56a52d7616b390255eb76";
    hash = "sha256-Y5UR0zHQlaXWWKEz8CDycQ5X5i1RxEDZyCRonHo+OiM=";
  };
}
