let username = "gaelan";
in {
  imports = [ ../modules/emacs ];

  config = {
    robot-disco.development-environment = {
      enable = true;

      fullname = "Gaelan D'costa";
      email = "gaelan@tulip.com";
      gpgKey = "814CF88EBD7287A1!";

      signCommits = true;
    };

    #robot-disco.emacs.enable = true;
    robot-disco.gnupg.enable = true;

    robot-disco.tulip.enable = true;

    home.stateVersion = "23.11";
    home.homeDirectory = "/Users/${username}";
    home.username = username;

    programs.zsh.enable = true;
  };
}
