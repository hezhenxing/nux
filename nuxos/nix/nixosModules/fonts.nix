{pkgs, ...}: {
  fonts = {
    packages = with pkgs; [
      noto-fonts-emoji
      noto-fonts-cjk-sans
      font-awesome
      symbola
      material-icons
      fira-code
      fira-code-symbols
      source-code-pro
      source-han-mono
      source-han-sans
      source-han-serif
      wqy_zenhei
    ];
  };
}
