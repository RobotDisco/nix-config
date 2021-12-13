# Set up a X11 window management environment
{ pkgs, lib, ... }:

{
  imports = [ ./hardware/yubikey/applications.nix ];
  
  # Useful graphical X11 programs
  environment.systemPackages = with pkgs; [
    # Notification toaster
    dunst
    # Display images and change X11 background
    feh
    # Network manager
    # TODO do we need a graphical + laptop .nix file?
    # EXWM is going to unconditionally run it...
    networkmanagerapplet
    # Control volume via pulseaudio
    # same concerns as above TODO...
    pasystray
    pavucontrol
    
    # Let's be nice and install some basic user applications
    # like a web browser
    chromium
    firefox
    nyxt
    qutebrowser
  ];

  services.xserver = {
    # Enable the X11 windowing system.
    enable = true;
    # Enable touchpad support.
    libinput.enable = true;
  };
  # We need _some_ desktopmanager, otherwise lightdm doesn't pick up our
  # .xsession file.
  services.xserver.desktopManager.xterm.enable = true;

  programs = {
    # Enable locking of machines (for when I step away)
    slock.enable = true;
    # Enable slock to be used by the login manager
    xss-lock = {
      enable = true;
      lockerCommand = "${pkgs.slock}/bin/slock";
    };
  };

  services = {
    # Enable X11 3D compositing managemr for graphics card acceleration
    picom.enable = true;
  };
}
