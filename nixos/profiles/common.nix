# Configuration that every nixos system I make should have.
{ pkgs, ... }:

{
  imports = [ ./hardware/yubikey/base.nix ];
  
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

  # Set regional settings I'm most likely in
  time.timeZone = "America/Toronto";
  i18n.defaultLocale = "en_CA.UTF-8";
  
  console = {
    # Honour the same settings as the linux console as in X11
    useXkbConfig = true;
  };
  # Caps Lock must die; replace with Ctrl
  services.xserver.xkbOptions = "ctrl:nocaps";
  

  environment = {
    # These are some handy misc. utilities to have around
    systemPackages = with pkgs; [
      file
      mkpasswd
      unzip
    ];
    # Allow zsh to autocomplete system packages, if installed
    pathsToLink = [ "/share/zsh" ];
  };

  hardware = {
    # Assume we have nonfree hardware
    enableRedistributableFirmware = true;

    # All my hardware is Intel, so
    cpu.intel.updateMicrocode = true;
  };

  # The global useDHCP flag is deprecated,
  # therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future,
  # so this generated config replicates the default behaviour.
  networking.useDHCP = false;
  
  nix = {
    # Enable nix flakes
    package = pkgs.nixUnstable;
    extraOptions = ''
        experimental-features = nix-command flakes
    '';

    # Enable binary cache downloads of standard nix packages
    binaryCaches = [
      "https://nix-community.cachix.org"
    ];
    binaryCachePublicKeys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  # Trust 'gaelan' account with nix packages
  nix.trustedUsers = [ "gaelan"];

  nixpkgs = {
    # Enable nonfree software
    config.allowUnfree = true;
  };

  services = {
    # tool for updating system firmware
    fwupd.enable = true;
  };
}
