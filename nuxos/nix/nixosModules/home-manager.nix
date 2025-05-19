{inputs, lib, ...}: {
  imports = [
    inputs.home-manager.nixosModules.home-manager
  ];
  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
    backupFileExtension = "bak";
    extraSpecialArgs = {
      inherit inputs;
    };
    users = with lib; with builtins; pipe ../home [
      readDir
      attrNames
      (map (removeSuffix ".nix"))
      (flip genAttrs (_: {
        home.stateVersion = "23.11";
        imports = attrValues inputs.self.homeModules;
      }))
    ];
  };
}
