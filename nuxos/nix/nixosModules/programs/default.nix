{inputs, pkgs, ...}:{
  programs = inputs.flakelight.lib.importDir ./.;
}
