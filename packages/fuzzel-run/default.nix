{
  fuzzel,
  writeShellApplication,
  lib,
  ...
}:
writeShellApplication {
  name = "fuzzel-run";
  runtimeInputs = [ fuzzel ];
  text = lib.readFile ./fuzzel-run.sh;
  meta = {
    description = "run command with fuzzel";
    licence = lib.licences.mit;
    platforms = lib.platforms.linux;
  };
}
