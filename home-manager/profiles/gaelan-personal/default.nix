let username = "gaelan";
in
{
  imports = [
    ./bitwarden.nix
    ../../modules/emacs
    ./games.nix
    ./gammastep.nix
    ./keyboard.nix
    ./productivity.nix
    ./ssh.nix
    ./web.nix
    ./yubikey.nix
    ./zsh.nix
  ];
  
  config = {
    home.stateVersion = "23.05";
    home.homeDirectory = "/home/${username}";
    home.username = username;

    robot-disco.development-environment = {
      enable = true;
      fullname = "Gaelan D'costa";
      email = "gdcosta@gmail.com";
      gpgKey = "814CF88EBD7287A1!";
      defaultBranch = "trunk";

      signCommits = true;
    };

    #robot-disco.emacs.enable = false;
    #robot-disco.emacs.enableExwm = false;

    robot-disco.gnupg.enable = true;

    robot-disco.services.seafile-client.enable = true;
  };
}
