{
  inputs.flakelight.url = "github:nix-community/flakelight";
  outputs = inputs @ {flakelight, ...}:
    flakelight ./. {
      inherit inputs;
      package = pkgs:
        pkgs.haskellPackages.developPackage {
          root = ./.;
        };
      devShell.packages = pkgs: let
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
          echo "  runvm   -- start NuxOS virtual machine"
        '';
      in
        with pkgs; [
          hpack
          cabal-install
          nixfmt
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
