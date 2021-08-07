# Support Yubikey hardware for using and for logging in.
{ pkgs, ... }:

{
  # Useful commandline packages
  environment.systemPackages = with pkgs; [
    yubikey-manager
    yubikey-personalization
    yubico-piv-tool
  ];
  
  services = {
    # Enable smart card (CCID) mode
    pcscd.enable = true;

    # udev rules needed by many yubikey apps
    udev.packages = [ pkgs.yubikey-personalization ];
  };

  programs = {
    # We'll need to use gpg-agent over sshagent for SSH key support.
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
    ssh.startAgent = false;
  };

  # Enable yubikey as a way to login
  security.pam.yubico = {
    enable = true;
    # Use a local challenge-response, not yubico's cloud service
    mode = "challenge-response";
    # TODO should I define user "gaelan"'s yubikey here?
  };
}
