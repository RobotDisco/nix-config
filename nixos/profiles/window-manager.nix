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
    # Music player
    playerctl
    # Screenshot taker
    scrot
    
    # Let's be nice and install some basic user applications
    # like a web browser
    chromium
    firefox
  ];

  services.xserver = {
    # Enable the X11 windowing system.
    enable = true;
    # Enable touchpad support.
    libinput.enable = true;
  };

  # EXWM is my window manager of choice, kludge in in via manual script
  # because I don't know how to make this idiomatically work otherwise.

  # TODO Play around with serviers.xserver.windowManager.exwm.loadScript
  # What if I moved the exwm-specific stuff out here? Load desktop-environment
  # and helm-exwm and configure them here? Assume use-package
  # possibly do do so manually.
  #
  # The EXWM package _does_ load user scripts afterwards, so it's viable!
  # I'd still have to worry about my emacs including local packages though...
  # How does the emacs-overlay play with home-manager?
  # services.xserver.windowManager.session = with pkgs; lib.singleton {
  #   name = "exwm";
  #   start = ''
  #     # Start emacs server
  #     exec ${dbus}/bin/dbus-launch --exit-with-session ${emacs}/bin/emacs
  #   '';
  # };
  # We need _SOME_ window manager enabled because lightdm fails if none are defined.
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
    # Enable mDNS service discovery
    # TODO Why did I need this again?
    # This possibly isn't even a window manager thing honestly
    # avahi.enable = true;
    
    # Enable X11 composite managemr for graphics card acceleration
    picom.enable = true;
  };
}
