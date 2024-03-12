{ config, pkgs, ... }:
{
  config = {
    home.packages =
      let
        compositor = config.nocturne.wayland.compositor.name;
        shell = config.nocturne.cli.shell.name;
        terminal = config.nocturne.wayland.terminal.name;
        editor = config.nocturne.wayland.editor.name;
        print-pretty-colors = pkgs.writeShellScriptBin "print-pretty-colors" ''
          printf " \033[31m \033[33m \033[32m \033[34m \033[36m \033[35m \033[0m"
        '';
        my-fetch = pkgs.writeShellScriptBin "my-fetch" ''
          cat ${./nocturne-swan}
          printf "  ╭─────────────╮\n" &&
          printf "  │ \033[31m \033[0m user     │ \033[31m$USER\033[0m\n" &&
          printf "  │ \033[33m \033[0m hostname │ \033[33m$(hostname)\033[0m\n" &&
          printf "  │ \033[32m \033[0m wm       │ \033[32m${compositor}\033[0m\n" &&
          printf "  │ \033[36m \033[0m shell    │ \033[36m${shell}\033[0m\n" &&
          printf "  │ \033[34m \033[0m terminal │ \033[34m${terminal}\033[0m\n"
          printf "  │ \033[35m󱇧 \033[0m editor   │ \033[35m${editor}\033[0m\n"
          printf "  ├─────────────┤\n"
          printf "  │   colors   │ $(${print-pretty-colors}/bin/print-pretty-colors)\n"
          printf "  ╰─────────────╯\n\n"
        '';
      in
      [ my-fetch ];
  };
}
