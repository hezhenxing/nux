{
  pkgs,
  config,
  ...
}: {
  boot = {
    kernelPackages = pkgs.linuxPackages_zen;
    kernelModules = ["v4l2loopback"];
    extraModulePackages = [
      config.boot.kernelPackages.v4l2loopback
    ];
    kernel.sysctl = {"vm.max_map_count" = 2147483642;};
    loader = {
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot/efi";
      };
      grub = {
        enable = true;
        device = "nodev";
        efiSupport = true;
        useOSProber = true;
        configurationLimit = 50;
      };
    };
    plymouth.enable = true;
  };
}
