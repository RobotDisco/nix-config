{ pkgs, lib, ... }:

{
  environment.systemPackages = with pkgs; [
    emacs
    xmobar
  ];
  
  services.xserver.windowManager.session = with pkgs; lib.singleton {
    name = "exwm";
    start = ''
      ${emacs}/bin/emacs -f exwm-enable --daemon
      ${xmobar}/bin/xmobar -d &
      ${emacs}/bin/emacsclient -c
    '';
  };
}
