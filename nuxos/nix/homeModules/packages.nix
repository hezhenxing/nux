{pkgs, ...}: {
  programs = {
    direnv.enable = true;
    helix = {
      enable = true;
      defaultEditor = true;
    };
  };
  home.packages = with pkgs; [
    devenv
    spotify
    tuner
    wechat-uos
    node2nix
  ];
}
