_:
let
  username = "gaelan";
in
{
  robot-disco = {
    development-environment = {
      enable = true;

      fullname = "Gaelan D'costa";
      email = "gaelan@tulip.com";
      gpgKey = "0x4B58E4871E1CA53A!";

      signCommits = true;
    };

    emacs.enable = true;
    gnupg.enable = true;

    tulip.enable = true;
  };

  home = {
    inherit username;
    homeDirectory = "/Users/${username}";
    # The state version is required and should stay at the version you
    # originally installed.
    stateVersion = "22.11";
  };

  programs.zsh.enable = true;
}
