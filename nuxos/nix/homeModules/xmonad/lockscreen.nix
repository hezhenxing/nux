{pkgs,...}:
{
  home.packages = with pkgs; [
    betterlockscreen
  ];
  services.screen-locker = {
    enable = true;
    lockCmd = "${pkgs.betterlockscreen}/bin/betterlockscreen -l dim";
    inactiveInterval = 5;
    xautolock = {
      enable = true;
      extraOptions = [
        "-notify 10"
        "-notifier \"${pkgs.libnotify}/bin/notify-send 'Locking in 10 seconds'\""
      ];
    };
  };
}