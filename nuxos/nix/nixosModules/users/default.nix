{inputs, lib, ...}: {
  users = {
    mutableUsers = true;
  };

  imports = with builtins; with lib; pipe ./. [
    inputs.flakelight.lib.importDir
    attrValues
  ];
}
