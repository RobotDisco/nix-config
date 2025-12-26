{ robotdisco-secrets, ... }:

let
  username = "gaelan";
in
{
  imports = [
    ./bitwarden.nix
    ./claude.nix
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
    age.rekey.hostPubkey = "${robotdisco-secrets}/users/gaelan-personal.pub";

    home = {
      inherit username;
      homeDirectory = "/home/${username}";
      # The state version is required and should stay at the version you
      # originally installed.
      stateVersion = "22.11";
    };

    robot-disco = {
      cdrip.enable = true;

      development-environment = {
        enable = true;
        fullname = "Gaelan D'costa";
        email = "gdcosta@gmail.com";
        gpgKey = "A517704FBD8D1018!";
        defaultBranch = "trunk";

        signCommits = true;
      };

      gnupg.enable = true;

      laptop.bluetoothID = 15;

      services.seafile-client.enable = true;

      wayland = {
        hyprland.enable = true;
      };
    };
  };
}
