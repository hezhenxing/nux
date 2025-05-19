{pkgs, ...}: {
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    amfora # Fancy Terminal Browser For Gemini Protocol
    brave
    brightnessctl
    cmatrix # Matrix Movie Effect In Terminal
    cowsay
    duf
    eza # Beautiful ls Replacement
    ffmpeg # Terminal Video / Audio Editing
    file-roller # Archive Manager
    gimp # Great photo Editor
    glxinfo # Needed for inxi diag util
    google-chrome
    htop
    hyprpicker # Color picker
    eog # For Image Viewing
    inxi # CLI System Information Tool
    just # Like make but simpler
    killall
    libnotify
    lm_sensors # Used for Getting Hardware Temps
    lolcat # Add Colors to Your Terminal Command Output
    lshw # Detailed Hardware Information
    mpv # Incredible Video player
    ncdu # Disk Usage Analyzer with Ncurses Interface
    nautilus # GUI File Manager
    nixfmt-rfc-style # Nix Formatter
    nwg-displays # Configure monitr configs via GUI
    onefetch # Provides build info on current system
    pavucontrol # For Editting Audio Levels & Devices
    pciutils
    picard # For Changing Music Metadata & Getting Cover Art
    pkg-config
    playerctl # Allows changing media volume through scripts
    ripgrep # Improved grep
    socat # Needed for screenshots
    unrar
    unzip
    usbutils
    v4l-utils # Used for things like OBS virtual camera
    wget
    yazi
    vscode
  ];
}
