{ pkgs, lib, ... }:

{
  environment.systemPackages = [
    pkgs.emacs
  ];
  
  services.xserver.windowManager.session = lib.singleton {
    name = "exwm";
    start = ''
      ${pkgs.emacs}/bin/emacsclient -c
    '';
  };
}
