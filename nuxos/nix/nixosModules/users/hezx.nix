{pkgs, ...}: {
  users = {
    groups.hezx.gid = 1000;
    users.hezx = {
      isNormalUser = true;
      description = "He Zhenxing";
      group = "hezx";
      extraGroups = [
        "docker"
        "libvirtd"
        "lp"
        "networkmanager"
        "scanner"
        "wheel"
      ];
      shell = pkgs.zsh;
      ignoreShellProgramCheck = true;
      initialPassword = "nuxos";
    };
  };
  nix.settings.allowed-users = ["hezx"];
  nix.settings.trusted-users = ["hezx"];
  security.sudo.extraRules = [
    {
      users = ["hezx"];
      commands = [
        {
          command = "ALL";
          options = ["NOPASSWD"];
        }
      ];
    }
  ];
}
