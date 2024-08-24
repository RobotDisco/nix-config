{ pkgs, ... }:

{
  # We need udev magic for my wonderfully proprietary Apple DVD burner
  # We have to send a series of messages to the device on connection in order
  # for it to wake up. Thanks Apple.
  # https://www.cmos.blog/use-apples-usb-superdrive-with-linux/comment-page-1/
  services.udev.extraRules = ''
    # Initialise Apple SuperDrive
    ACTION=="add", ATTRS{idProduct}=="1500", ATTRS{idVendor}=="05ac", DRIVERS=="usb", RUN+="${pkgs.sg3_utils}/bin/sg_raw /dev/$kernel EA 00 00 00 00 00 01"
  '';
}
