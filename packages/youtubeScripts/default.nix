{ lib
, stdenv
, makeWrapper
, chafa
, coreutils
, gnugrep
, mpv
, wl-clipboard
, ytfzf
, yt-dlp
}:

with lib;

stdenv.mkDerivation rec {
  name = "youtubeScripts";
  version = "1.0";
  src = ./scripts;

  nativeBuildInputs = [ makeWrapper ];
  runtimeDependencies = [
    chafa
    coreutils
    gnugrep
    mpv
    wl-clipboard
    ytfzf
    yt-dlp
  ];

  dontUnpack = true;
  dontBuild = true;
  dontConfigure = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    for script in ytd ytdp ytp ytu; do
      install -Dm 0755 $src/$script $out/bin
      wrapProgram $out/bin/$script --set PATH \
        "${
          makeBinPath runtimeDependencies
        }"
    done
    
    runHook postInstall
  '';

  meta = {
    description = "Simple Test Scripts";
    license = licenses.mit;
    platforms = platforms.all;
  };
}
