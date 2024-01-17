{ pkgs, ... }:

{
  imports = [
    ./fileserver.nix
  ];

  config = {
    # Managed home directories + Gaelan's HM customizations.
    home-manager.users.gaelan =
      import ../../home-manager/profiles/gaelan-personal;

    # Gaelan owns this machine, so let him do nix stuff.
    nix.settings.trusted-users = [ "gaelan" ];

    # Enable ZSH
    programs.zsh.enable = true;

    # Define the gaelan user
    users.users.gaelan = {
      description = "Gaelan D'costa";
      isNormalUser = true;
      home = "/home/gaelan";
      group = "users";
      createHome = true;
      # Gaelan should have access to network, sound, sudo, video and docker
      extraGroups = [ "networkmanager" "wheel" ];
      # Make sure Gaelan's yubikey can ssh into this machine
      openssh.authorizedKeys.keyFiles = [ ./yubikey.pub ];
      # Gaelan uses the Z Shell.
      shell = pkgs.zsh;
    };
  };
}
