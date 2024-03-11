{ config, lib, pkgs, ... }: let
  way-ed = config.nocturne.wayland.editor;
  cli-ed = config.nocturne.cli.editor;
in {
  config = lib.mkMerge [
    # Terminal Text Editor
    { home.packages = with pkgs; [ vim ]; }

    # Use Terminal Editor
    (lib.mkIf (config.nocturne.cli.editor.follow == "cli") {
      home.sessionVariables.EDITOR = cli-ed.name;
    })

    # Use Wayland Editor, Reuse Command Prioritized
    (lib.mkIf ((cli-ed.follow == "wayland") && (way-ed.exec-reuse != null)) {
      home.sessionVariables.EDITOR = way-ed.exec-reuse;
    })
    (lib.mkIf ((cli-ed.follow == "wayland") && (way-ed.exec-reuse == null)) {
      home.sessionVariables.EDITOR = way-ed.exec;
    })
  ];
}
