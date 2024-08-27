{
  boot = {
    loader = {
      #systemd-boot.enable = true;

      # This is what everyone sets, unsure why.
      efi.canTouchEfiVariables = true;

      grub = {
        # In order to lean on mirrored boot volumes, we use grub as our
        # bootloader.
        enable = true;
        # Enable GPT partition support
        efiSupport = true;

        # Use two boot volumes, mirrored. This is to keep our disks totally
        # mirrored, and try to mirror our boot volumes the way we
        # (straightforwardly) mirror our main disk volumes.
        mirroredBoots = [
          {
            devices = [ "nodev" ];
            path = "/boot0";
            efiSysMountPoint = "/boot0";
          }
          {
            devices = [ "nodev" ];
            path = "/boot1";
            efiSysMountPoint = "/boot1";
          }
        ];
      };
    };
  };
}
