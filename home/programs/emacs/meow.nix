{
  fetchFromGitHub,
  fakeHash,
  trivialBuild,
}:
trivialBuild rec {
  pname = "emacs-meow";
  version = "1.6.0";
  src = fetchFromGitHub {
    owner = "meow-edit";
    repo = "meow";
    rev = "cde5f4c57bc8657bd361f60725338cf218b66e7d";
    hash = "sha256-w9JBdFFb/H3kLfzfpN4ZmBNV509E5+PRddfikvieyco=";
  };
}
