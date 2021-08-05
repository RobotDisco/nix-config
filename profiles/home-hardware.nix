# Support for devices around the house
{ pkgs, ... }:

{
  # Enable CUPS to print documents.
  services.printing.enable = true;

  hardware.sane.enable = true;
  nixpkgs.config.sane.extraFirmware = [
    {
      src = pkgs.fetchurl {
        # http://www.openfusion.net/linux/scansnap_1300i
        url = "http://www.openfusion.net/public/files/1300i_0D12.nal";
        sha256 = "cbea48c6cee675c2ea970944b49b805d665ee659f753a50b83c176973f507591";
      };
      name = "1300i_0D12.nal";
      backend = "epjitsu";
    }
  ];

  # We need udev magic for my wonderfully proprietary Apple DVD burner
  environment.systemPackages = [
    pkgs.sg3_utils
  ];
  services.udev.extraRules = ''
# Initialise Apple SuperDrive
ACTION=="add", ATTRS{idProduct}=="1500", ATTRS{idVendor}=="05ac", DRIVERS=="usb", RUN+="${pkgs.sg3_utils}/bin/sg_raw /dev/$kernel EA 00 00 00 00 00 01"
'';
}
