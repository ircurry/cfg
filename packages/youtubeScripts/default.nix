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
    install -Dm 0755 $src/ytu $out/bin
    wrapProgram $out/bin/ytu --set PATH \
      "${
        makeBinPath runtimeDependencies
      }"
    install -Dm 0755 $src/ytd $out/bin
    wrapProgram $out/bin/ytd --set PATH \
      "${
        makeBinPath runtimeDependencies
      }"
    install -Dm 0755 $src/ytdp $out/bin
    wrapProgram $out/bin/ytdp --set PATH \
      "${
        makeBinPath runtimeDependencies
      }"
    install -Dm 0755 $src/ytp $out/bin
    wrapProgram $out/bin/ytp --set PATH \
      "${
        makeBinPath runtimeDependencies
      }"
    
    runHook postInstall
  '';

  meta = {
    description = "Simple Test Scripts";
    license = licenses.mit;
    platforms = platforms.all;
  };
}
