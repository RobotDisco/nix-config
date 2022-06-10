{ pkgs, ... }:

{
  services = {
    # Enable smart card (CCID) mode
    pcscd.enable = true;

    # udev rules often needed for yubikey support
    udev.packages = [ pkgs.yubikey-personalization ];
  };

  # Enable yubikey as a way to login
  security.pam.yubico = {
    enable = true;
    # Use a local challenge-response, not yubico's cloud service
    mode = "challenge-response";
  };
}