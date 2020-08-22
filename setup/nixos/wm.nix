{ pkgs, lib, ... }:

{
  environment.systemPackages = with pkgs; [
    emacs
  ];
  
  services.xserver.windowManager.session = with pkgs; lib.singleton {
    name = "exwm";
    start = ''
      ${emacs}/bin/emacs -f exwm-enable --daemon
      ${emacs}/bin/emacsclient -c
    '';
  };
}
