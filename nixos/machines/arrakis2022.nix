{
  imports = [
    ../profiles/audio.nix
    ../profiles/boot.nix
    ../profiles/firmware.nix
    ../profiles/hidpi.nix
    ../profiles/home-devices
    ../profiles/keyboard.nix
    ../profiles/laptop
    ../profiles/nix.nix
    ../profiles/regional.nix
    ../profiles/security.nix
    ../profiles/ssd.nix
    ../profiles/steam.nix
    ../profiles/virtualbox.nix
    ../profiles/window-manager/wayland.nix
    ../profiles/yubikey.nix
    ../../users/gaelan
  ];

  config = {
    boot.initrd.availableKernelModules =
      [ "xhci_pci" "thunderbolt" "nvme" "usb_storage" "usbhid" "sd_mod" ];
    boot.kernelModules = [ "dm-snapshot" ];

    networking.hostName = "arrakis";

    # Enable the Framework networking device
    networking.interfaces.wlp170s0.useDHCP = true;

    # Support thunderbolt
    services.hardware.bolt.enable = true;

    # Framework firmware is in the lvfs-testing repo
    services.fwupd.extraRemotes = [
      "lvfs-testing"
    ];
  };
}
