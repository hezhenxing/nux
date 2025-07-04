{
  inputs.flakelight-haskell.url = "github:hezhenxing/flakelight-haskell";
  outputs =
    inputs@{ flakelight-haskell, ... }:
    flakelight-haskell ./. {
      inherit inputs;
      devShell.packages =
        pkgs:
        let
          repl = pkgs.writeShellScriptBin "repl" ''
            hpack
            cabal repl
          '';
          runtest = pkgs.writeShellScriptBin "runtest" ''
            hpack
            cabal test
          '';
          nux = pkgs.writeShellScriptBin "nux" ''
            export NUX_FLAKE=$(dirname $(dirname $out))
            export NUXOS_FLAKE=$NUX_FLAKE/nuxos
            cd $NUX_FLAKE
            nix run . -- "$@"
          '';
          help = pkgs.writeShellScriptBin "help" ''
            echo "Welcome to Nux!"
            echo "Available commands:"
            echo "  repl    -- start cabal repl"
            echo "  runtest -- run cabal test"
            echo "  nux     -- build and run nux command"
          '';
        in
        with pkgs;
        [
          zlib # required by rio-orphans
          repl
          runtest
          nux
          help
        ];
      template = {
        path = ./nuxos;
        description = "Template for NuxOS system";
      };
    };
}
