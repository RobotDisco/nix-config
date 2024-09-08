{ pkgs, ... }:

{
  services = {
    # Enable smart card (CCID) mode
    pcscd.enable = true;

    udev = {
      # udev rules often needed for yubikey support
      packages = [ pkgs.yubikey-personalization ];

      # Lock the laptop when Yubikey is unplugged
      extraRules = ''
        ACTION=="remove",\
          ENV{ID_BUS}=="usb",\
          ENV{ID_MODEL_ID}=="0407",\
          ENV{ID_VENDOR_ID}=="1050",\
          ENV{ID_VENDOR}=="Yubico",\
          RUN+=${pkgs.systemd}/bin/loginctl lock-sessions"
      '';
    };
  };

  # Enable yubikey as a way to login (via U2F)
  security.pam = {
    u2f = {
      enable = true;
      # Prompt for the u2f device.
      cue = true;
      # Require yubikey auth as well as passwords
      # control = "required";
    };
  };
}
