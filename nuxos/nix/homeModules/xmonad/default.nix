{inputs, ...}: let
  inherit (builtins) attrValues;
  inherit (inputs.flakelight.lib) importDir;
in {
  imports = attrValues (importDir ./.);
}
