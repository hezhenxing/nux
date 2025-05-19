{
  # polybar service requires tray.target, which can be created by enable sddm.
  services.polybar = {
    enable = true;
    script = "";
    config = ./polybar/config.ini;
  };
  # also need to enable xsession
  xsession.enable = true;
}

