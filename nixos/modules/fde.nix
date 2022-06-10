{
  # Necessary modules for 2FA support
  boot.initrd.kernelModules = [ "vfat" "nls_cp437" "nls_iso8859-1" "usbhid" ];

  # Enable (and require) Yubikey on bootup as 2FA.
  boot.initrd.luks.yubikeySupport = true;
  boot.initrd.luks.devices = {
    cryptdata = {
      # The unencrypted boot device holding our salt
      device = "/dev/nvme0n1p2";
      # Since our LVM is encrypted, defer LVM mounting until later.
      preLVM = true;

      # value SSD performance and drive lifetime over security
      allowDiscards = true;
      bypassWorkqueues = true;

      yubikey = {
        slot = 2;
        twoFactor = true;
        storage = { device = "/dev/nvme0n1p1"; };
      };
    };
  };
}