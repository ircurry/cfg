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
    rev = "73f2dd2fce8069c4ac4f6ae2a8afdda40450a7e4";
    hash = "sha256-Ief014W0XHQlYfTrvqykBJCXp/6bYJeT+ikYjkHFbLE=";
  };
}
