# Nux

Nux simplifies managing NuxOS (https://github.com/hezhenxing/NuxOS, a NixOS-based distribution), making it accessible to beginners while preserving NixOS's flexibility and customization. NuxOS leverages the NixOS ecosystem and aims to provide a more familiar experience akin to popular operating systems.

Warning: This project is in early development. Features may break or change significantly. Not recommended for production use.

Note: NuxOS only support boot via EFI, and uses GRUB2 as the boot loader, os-probe is enabled, it should be OK to install NuxOS along side with other operating systems.

## Installation

You can get the Nux tool from the flake output:

```shell
nix shell github:hezhenxing/nux
```

## NuxOS Installation

First use `os init` command to initialize a NuxOS configuration:

```shell
nux os init nuxos-config --profile kde --language zh_CN.UTF-8 --timezone Asia/Shanghai
```

It will create a `nuxos-config` directory in current directory and initialize a NuxOS configuration with given profile (a predefined set of environments, other support profiles: gnome, budgie, hyprland, xmonad, etc.). It will also automatically add a host and a user to the configuration base on your current host name and user name. If you don't like it, you can modify them with `host` and `user` commands or specify host and user options when issuing the `os init` command.

```shell
nux os init nuxos-config --profile gnome --user harry --description "Harry Potter" --email "harry.potter@hogwarts.edu"
```

Please use `--help` to looking for more information of available commands and options.

After initialization, you can install the NuxOS to your machine. There are two ways to install the system, one is to replace your current NixOS system:

```
nux os install nuxos-config -y
```

Without `-y`, it will ask for comfirmation before actually start the build and installation of the system.

Another way is to install to a new root partition:

```
nux os install nuxos-config --root /dev/<ROOT PARTITION>
```

This will use the configuration of host of current host name (outpout of `hostname` command), if this is not the case, you can use `--host` option to specify the host name of the configuration to use.

Replace `<ROOT PARTITION>` with your actually root partition device name.

The partition should be already formated, if not you can specify `--format` option to format it before install the system. And you may also need `--force` if you want to force the formatting even the partition already has a filesystem, Please be careful when using these options.

Warning: Please make sure you choose the right parition, especially with forced formatting options enabled, or you may loose data or make your whole machine unusable.

Note: Installation will use `/mnt` to mount the root device, please make sure it exist and not used by other programs. May make this configurable later.

The installation may take a few minutes, dependent on the system size and networking speed.

After installation is succeeded, it should also install the GRUB boot loader, and you can reboot the system and choose the boot item labelled `NuxOS`.

By default, the installation will copy the configuration, the whole directory of nuxos-config, to `/etc/nuxos`, and then after login to the system, you need to use `sudo` to install packages or run other commands that need to modify the configuration.

If you don't want copy the configuration to system, you can add '--link' option to create a symbolic link instead, and then you can avoid using `sudo` to install packages or run any other commands that need to change the configuration.

```shell
nux os install nuxos-config --link --root /dev/<ROOT PARTITION>
```

Warning: When installing to another partition with `--link` option, you have to make sure the `nuxos-config` directory is accessible in the new system. You can achieve this by copying the `nuxos-config` directory to the installed root directory `/mnt` with the same path, or using a shared partition and adding it to the new system with the same mount point.

## Package Management

After boot and log into NuxOS system (default password is `nuxos`), you can use `pkg install` command to install additional packages to the system.

```shell
nux pkg install vscode helix
```

By default, the packages will be installed to the current user, if you want to install the package system wide, add `--global` or `-g` option.

NuxOS uses home-manager for user package management. It prioritizes NixOS modules, services, programs, and then pkgs when resolving package names. This means system and user packages, even if identically named, might have different configurations and behaviors. Future improvements will aim for consistent package behavior across system and user installations.

## Host Management

Nux support managing multiple hosts in one configuration, you can use `host` command to add, delete or listing hosts.

```shell
nux host add host1
```

This will add a new host configuration directory `host1` in the `nix/hosts` directory. You can add your own NixOS customization module files in this host directory, and they will be automatically loaded.

## User Management

Nux allows you mananging multiple users in one configuration, and they will be included for all configured hosts.

```shell
nux user add user1 <USERNAME>
```

This will add a new user configuration directory `user` in the `nix/users` directory. You can add your own Home Manager configuration modules in this directory, and they will be automatically loaded for the user.

You can use `--help` to check for more information.

## Hacking

Any kinds of contributions are welcome. You can contribute to both this project and the NuxOS (https://github.com/hezhenxing/NuxOS) project.

It is recommended to use nix and direnv for the development environment. If you're using vscode, you can use the Nix Environment Selector (arrterian.nix-env-selector) plugin, make sure you enable the flake support.

## Attributions

The amazing NixOS ecosystem is what makes this possible, I would like to especially acknowlege:

  - Modular Nix flake framework [flakelight](https://github.com/nix-community/flakelight)
  - Modern helper utility [NH](https://github.com/nix-community/nh)
