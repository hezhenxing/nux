{inputs, flakelight, ...}: {
  system = "x86_64-linux";
  modules = with builtins;
    attrValues inputs.self.nixosModules
    ++ (attrValues (flakelight.importDir ./nux));
}
