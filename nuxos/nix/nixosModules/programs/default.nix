{inputs, ...}:{
  programs = inputs.flakelight.lib.importDir ./.;

  imports = [
    {
      programs.i3lock.enable = true;
    }
  ];
}
