{lib, inputs, ...}:
{
  imports = with builtins; with lib; pipe ./. [
    inputs.flakelight.lib.importDir
    attrValues
  ];
}
