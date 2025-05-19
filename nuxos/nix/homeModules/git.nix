{
  programs.git = {
    enable = true;
    ignores = [
      "*~"
      "*.swp"
      ".vscode/"
      "result"
      "*.qcow2"
    ];
    extraConfig = {
      init = {
        defaultBranch = "main";
      };
    };
  };
}
