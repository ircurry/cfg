{ host, ... }:
{
  nocturne.cli.scripts = {
    nrepl = ''
      if [[ -f ./repl.nix ]]; then
        nix repl --arg host '"${host}"' --file ./repl.nix "$@"
      elif [[ -f ./flake.nix ]]; then
        nix repl --expr 'builtins.getFlake (builtins.toString ./.)' "$@"
      elif [[ -d "$NOCTURNE_CONF_DIR" ]]; then
        pushd "$NOCTURNE_CONF_DIR" > /dev/null
        nix repl --arg host '"${host}"' --file ./repl.nix "$@"
        popd > /dev/null
      else
        echo "unable to start repl"
        exit 1
      fi
    '';
  };
}
