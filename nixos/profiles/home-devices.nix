# Support for various devices found around the house
{ pkgs, ... }:

{
  # Enable CUPS to print documents, since I have a printer
  services.printing = {
    enable = true;
    drivers = [ pkgs.brlaser ];
  };
  hardware.printers = {
    ensurePrinters = [{
      name = "Brother_HL-2240D_series";
      location = "Living Room";
      deviceUri = "usb://Brother/HL-2240D%20series?serial=E1J801274";
      model = "drv:///brlaser.drv/br2240d.ppd";
      ppdOptions = {
        job-sheets = "none, none";
        media = "na_letter_8.5x11in";
	sides = "two-sided-long-edge";
      };
    }
  ];
  ensureDefaultPrinter = "Brother_HL-2240D_series";
};

  # I have a ScanSnap scanner. It has proprietary firmware...
  hardware.sane.enable = true;
  nixpkgs.config.sane.extraFirmware = [{
    src = pkgs.fetchurl {
      # http://www.openfusion.net/linux/scansnap_1300i
      url = "http://www.openfusion.net/public/files/1300i_0D12.nal";
      sha256 =
        "cbea48c6cee675c2ea970944b49b805d665ee659f753a50b83c176973f507591";
    };
    name = "1300i_0D12.nal";
    backend = "epjitsu";
  }];

  # We need udev magic for my wonderfully proprietary Apple DVD burner
  services.udev.extraRules = ''
    # Initialise Apple SuperDrive
    ACTION=="add", ATTRS{idProduct}=="1500", ATTRS{idVendor}=="05ac", DRIVERS=="usb", RUN+="${pkgs.sg3_utils}/bin/sg_raw /dev/$kernel EA 00 00 00 00 00 01"
  '';

  # Remap buttons for my kensington trackball
  # NOTE For xserver config limitation reasons I can see this moving into a
  # dedicated module, it doesn't seem composable.
  services.xserver.config = ''
    Section "InputClass"
      Identifier "Kensington Expert Mouse"
      MatchUSBID "047d:1020"
      Driver "libinput"
      Option "ButtonMapping" "1 2 8 4 5 6 7 3 9"
    EndSection
  '';

  # One of my workhorse keyboards has configuration software.
  hardware.keyboard.uhk.enable = true;
   
  environment.systemPackages = [
    pkgs.sg3_utils
    pkgs.uhk-agent
  ];
}
