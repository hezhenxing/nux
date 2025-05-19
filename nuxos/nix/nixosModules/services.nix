{
  services = {
    libinput.enable = true;
    fstrim.enable = true; # SSD optimizer
    gvfs.enable = true; # For mounting USB & More
    openssh.enable = true;
    blueman.enable = true;
    tumbler.enable = true;
    gnome.gnome-keyring.enable = true;
    smartd = {
      enable = true;
      autodetect = true;
    };
    pipewire = {
      enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      pulse.enable = true;
    };
  };
}
