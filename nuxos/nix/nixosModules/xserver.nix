{
  services.xserver = {
    enable = true;
    dpi = 120;
    xkb = {
      layout = "us";
      variant = "";
    };
    displayManager = {
      gdm.enable = true;
      startx.enable = true;
    };
    windowManager.xmonad = {
      enable = true;
    };
  };
  services.libinput = {
    enable = true;
    touchpad.naturalScrolling = true;
  };
}
