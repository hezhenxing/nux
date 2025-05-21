{inputs, ...}: {
  system = "x86_64-linux";
  modules =
    builtins.attrValues inputs.self.homeModules
    ++ [{imports = [inputs.stylix.homeModules.stylix];}];
}
