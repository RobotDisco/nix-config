# My preferred window manager is EXWM
{ pkgs, lib, ... }:

{
  environment.systemPackages = with pkgs; [
    brightnessctl
    dbus
    dunst
    emacs
    feh
    networkmanagerapplet
    pasystray
    pavucontrol
    playerctl
    scrot
    slock
    stalonetray
    xmobar
  ];

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  
  services.xserver.windowManager.session = with pkgs; lib.singleton {
    name = "exwm";
    start = ''
      # Start emacs server
      exec ${dbus}/bin/dbus-launch --exit-with-session ${emacs}/bin/emacs
    '';
  };

  programs.xss-lock = {
    enable = true;
    lockerCommand = "${pkgs.slock}/bin/slock";
  };

  services.avahi.enable = true;
  services.picom.enable = true;
  services.blueman.enable = true;

  # Enable locking of machines (for when I step away)
  programs.slock.enable = true;
}
