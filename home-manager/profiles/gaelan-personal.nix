let username = "gaelan";
in {
  robot-disco.development-environment = {
    enable = true;
    fullname = "Gaelan D'costa";
    email = "gdcosta@gmail.com";
    gpgKey = "814CF88EBD7287A1!";

    signCommits = true;
  };
  robot-disco.emacs.enable = true;
  robot-disco.emacs.enableExwm = true;
  robot-disco.games.enable = true;
  robot-disco.gnupg.enable = true;

  home.stateVersion = "23.05";
  home.homeDirectory = "/home/${username}";
  home.username = username;

  services.seafile-client.enable = true;
}
