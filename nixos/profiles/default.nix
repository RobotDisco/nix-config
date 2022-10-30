{ ... }:

{
  imports = [
    ./audio.nix
    ./laptop.nix    
    ./boot.nix
    ./common.nix
    ./steam.nix
    ./window-manager.nix
    ./yubikey.nix
    ./user/gaelan.nix
  ];
}
