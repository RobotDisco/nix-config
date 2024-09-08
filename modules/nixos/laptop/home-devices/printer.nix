{ pkgs, ... }:

{
  # Enable CUPS to print documents, since I have a printer
  services.printing = {
    enable = true;
    drivers = [ pkgs.brlaser ];
  };
  hardware.printers = {
    ensurePrinters = [
      {
        name = "Brother_HL-2240D_series";
        location = "Living Room";
        deviceUri = "usb://Brother/HL-2240D%20series?serial=E1J801274";
        model = "drv:///brlaser.drv/br2270dw.ppd";
        ppdOptions = {
          job-sheets = "none, none";
          media = "na_letter_8.5x11in";
          sides = "two-sided-long-edge";
        };
      }
    ];
    ensureDefaultPrinter = "Brother_HL-2240D_series";
  };
}
