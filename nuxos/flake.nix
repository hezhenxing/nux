{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flakelight = {
      url = "github:nix-community/flakelight";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    stylix = {
      url = "github:danth/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };
    nvf = {
      url = "github:notashelf/nvf";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = inputs@{flakelight, ...}: flakelight ./. {
    inherit inputs;
    nixpkgs.config = { allowUnfree = true; };
    apps.vm = {
      type = "app";
      program = "${inputs.self.nixosConfigurations.nux.config.system.build.vm}/bin/run-nux-vm";
    };
  };
}
