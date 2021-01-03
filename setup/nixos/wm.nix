{ pkgs, lib, ... }:

{
  environment.systemPackages = with pkgs; [
    emacs
    xmobar
    networkmanagerapplet
    dunst
    pasystray
  ];
  
  services.xserver.windowManager.session = with pkgs; lib.singleton {
    name = "exwm";
    start = ''


      # Start status bar
      ${xmobar}/bin/xmobar -d &

      # Start emacs server
      exec dbus-launch --exit-with-session emacsclient -c
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
