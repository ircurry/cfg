{
  fetchFromGitHub,
  lib,
  stdenv,
}:
stdenv.mkDerivation {
  name = "id3";
  version = "0.81";

  src = fetchFromGitHub {
    owner = "squell";
    repo = "id3";
    rev = "0.81";
    hash = "sha256-+h1wwgTB7CpbjyUAK+9BNRhmy83D+1I+cZ70E1m3ENk=";
  };

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    mkdir -p $out/share/man/man1
    make prefix="$out" mandir="$out/share/man" install
    make prefix="$out" bash_completion

    runHook postInstall
  '';
}
