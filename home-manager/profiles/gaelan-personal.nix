{
  username = "gaelan";
  system = "x86_64-linux";
  configuration = {
    robot-disco.development-environment = {
      enable = true;
      fullname = "Gaelan D'costa";
      email = "gdcosta@gmail.com";
      gpgKey = "814CF88EBD7287A1!";
    };
    robot-disco.emacs.enable = true;
    robot-disco.emacs.enableExwm = true;
    robot-disco.games.enable = true;
    robot-disco.gnupg.enable = true;
  };
}
