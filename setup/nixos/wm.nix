{ pkgs, lib, ... }:

{
  environment.systemPackages = with pkgs; [
    dunst
    emacs
    feh
    networkmanagerapplet
    pasystray
    pavucontrol
    slock
    stalonetray
    xmobar
  ];
  
  services.xserver.windowManager.session = with pkgs; lib.singleton {
    name = "exwm";
    start = ''
      # Start emacs server
      exec dbus-launch --exit-with-session emacs
    '';
  };

  programs.xss-lock = {
    enable = true;
    lockerCommand = "${pkgs.slock}/bin/slock";
  };

  services.avahi.enable = true;
  services.picom.enable = true;
  services.blueman.enable = true;
}
