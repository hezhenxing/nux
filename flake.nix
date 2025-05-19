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
      runvm = pkgs.writeShellScriptBin "runvm" ''
        nix run ./nuxos#nixosConfigurations.nux.config.system.build.vm
      '';
    in with pkgs; [
      hpack
      cabal-install
      nixfmt
      repl
      runtest
      runvm
    ];
    template = {
      path = ./nuxos;
      description = "Template for NuxOS system";
    };
  };
}
