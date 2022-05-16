{
  description = "Gaelan's nix-based systems configuration";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixos-21.11;
    nixos-hardware.url = github:NixOS/nixos-hardware/master;
  };

  outputs = { self, nixpkgs, nixos-hardware }: {
    nixosConfigurations = {
      arrakis2022 = {
        system = "x86_64-linux";
        modules = [
          nixpkgs.nixosModules.notDetected
          nixos-hardware.nixosModules.framework
          ./nixos/machines/arrakis2022.nix
        ];
      };
    };
  };
}
