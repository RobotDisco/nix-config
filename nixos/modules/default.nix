{ ... }:

{
  imports = [
    ./audio.nix
    ./boot.nix
    ./common.nix
    ./framework.nix
    ./laptop.nix
    ./steam.nix
    # ./user-gaelan.nix
    ./window-manager.nix
    ./yubikey.nix
  ];
}
