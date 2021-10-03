# Common configuration for baremetal installations
{ pkgs, ... }:

{
  imports = [ ./hardware/yubikey/base.nix ];

  hardware = {
    # Assume we have nonfree hardware
    enableRedistributableFirmware = true;
    # All my hardware is Intel, so
    cpu.intel.updateMicrocode = true;
  };

  services = {
    # tool for updating system firmware
    fwupd.enable = true;
  };

  # Get emails for any hard drive failures
  services.smartd = {
    enable = true;
    notifications = {
      mail.sender = "root@robot-disco.net";
      mail.enable = true;
      mail.recipient = "gdcosta@gmail.com";
    };
    # Don't care about most drives as much as My NAS drives
    # wekly short tests, monthly long tests
    defaults.autodetected = "-a -o on -s (S/../../3/12|L/../01/./17)";
  };
}
