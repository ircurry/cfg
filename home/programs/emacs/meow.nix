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
    rev = "96fecf530d7b29dfb66f327933ebe37f016df132";
    hash = "sha256-xax0UJi6LiYwYvGfX9o9QPraOrwBJbpSSMn8cux9g4U=";
  };
}
