{
  inputs.flakelight.url = "github:nix-community/flakelight";
  outputs = inputs@{flakelight, ...}: flakelight ./. {
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
      run = pkgs.writeShellScriptBin "run" ''
        nix run . -- "$@"
      '';
      runvm = pkgs.writeShellScriptBin "runvm" ''
        nix run ./nuxos#nixosConfigurations.nuxos.config.system.build.vm
      '';
      help = pkgs.writeShellScriptBin "help" ''
        echo "Welcome to Nux!"
        echo "Available commands:"
        echo "  repl    -- start cabal repl"
        echo "  runtest -- run cabal test"
        echo "  runvm   -- start NuxOS virtual machine"
      '';
    in with pkgs; [
      hpack
      cabal-install
      nixfmt
      repl
      runtest
      runvm
      run
      help
    ];
    template = {
      path = ./nuxos;
      description = "Template for NuxOS system";
    };
  };
}
