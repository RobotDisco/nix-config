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
}
