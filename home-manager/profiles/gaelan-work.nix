{
  username = "gaelan.dcosta";
  system = "x86_64-darwin";
  configuration = {
    robot-disco.development-environment = {
      enable = true;
      fullname = "Gaelan D'costa";
      email = "gaelan@tulip.com";
      gpgKey = "814CF88EBD7287A1!";
    };
      
    robot-disco.emacs.enable = true;
    robot-disco.gnupg.enable = true;
  };
}
