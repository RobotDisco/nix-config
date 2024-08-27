{ ... }:

let
  btrfs-options = [
    "compress=zstd"
    "noatime"
  ];
in
{
  imports = [
    ./hardware-configuration.nix

    ../../modules/nixos/android.nix
    ../../modules/nixos/audio.nix
    ../../modules/nixos/firmware.nix
    ../../modules/nixos/hidpi.nix
    ../../modules/nixos/home-devices
    ../../modules/nixos/keyboard.nix
    ../../modules/nixos/laptop
    ../../modules/nixos/nix.nix
    ../../modules/nixos/regional.nix
    ../../modules/nixos/security.nix
    ../../modules/nixos/ssd.nix
    ../../modules/nixos/steam.nix
    ../../modules/nixos/yubikey.nix

    ../../modules/nixos/window-manager/wayland.nix

    ../../users/gaelan
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "arrakis";

  # Filesystem overrides
  fileSystems."/".options = btrfs-options;
  fileSystems."/nix".options = btrfs-options;
  fileSystems."/home".options = btrfs-options;
  fileSystems."/boot".options = [ "noatime" ];

  # Simple password-based Full Disk Encryption
  boot.initrd.luks.devices = {
    nixoscrypt = {
      device = "/dev/nvme0n1p2";

      # I'd rather have TRIM support than perfect security
      allowDiscards = true;
      # increase performance on SSDs
      bypassWorkqueues = true;

      # Set to false if you need things like networking to happen first
      preLVM = true;
    };
  };

  # Enable dynamic configuration of primary NIC
  networking.interfaces.wlp1s0.useDHCP = true;

  # Support thunderbolt manager software
  services.hardware.bolt.enable = true;

  # Framework firmware is in the lvfs-testing repo
  services.fwupd.extraRemotes = [ "lvfs-testing" ];

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "21.11"; # Did you read the comment?
}
  
