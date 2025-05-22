{
  services.picom = {
    enable = true;
    activeOpacity = 1.0;
    inactiveOpacity = 0.5;
    backend = "glx";
    fade = true;
    fadeDelta = 5;
    shadow = true;
    shadowOpacity = 0.75;
    wintypes = {
      popup_menu.opacity = 0.9;
      dropdown_menu.opacity = 0.9;
    };
    opacityRules = [
      "100:name *= 'betterlockscreen'"
      "100:fullscreen"
      "100:name *= 'rofi'"
    ];
  };
}
