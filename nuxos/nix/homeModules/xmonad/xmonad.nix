{
  xsession = {
    enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = hp: with hp; [
        dbus
        List
        monad-logger
      ];
      config = ./xmonad.hs;
    };
  };
  systemd.user.targets.hm-graphical-session.Unit = {
    Requires = [
      "xdg-desktop-autostart.target"
    ];
  };
}
