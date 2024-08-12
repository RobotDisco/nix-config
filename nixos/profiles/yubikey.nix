{ pkgs, ... }:

{
  services = {
    # Enable smart card (CCID) mode
    pcscd.enable = true;

    # udev rules often needed for yubikey support
    udev.packages = [ pkgs.yubikey-personalization ];
  };

  # Enable yubikey as a way to login (via U2F)
  security.pam = {
    services = {
      login.u2fAuth = true;
      sudo.u2fAuth = true;      
    };
    u2f = {
      enable = true;
      # Prompt for the u2f device.
      cue = true;
      # Require yubikey auth
      control = "required";
    };
  };
}
