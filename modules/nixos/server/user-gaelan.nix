{pkgs, ... }: 

let
  username = "gaelan";
  fullname = "Gaelan D'costa";
  id = 1000;
in {
  # Enable ZSH (this should be placed more globally)
  programs.zsh.enable = true;
  
  # Gaelan owns this machine, so let him do nix stuff.
  nix.settings.trusted-users = [ username ];

  # Define the gaelan group
  users.groups."${username}".gid = id;
  
  # Define the gaelan user
  users.users."${username}" = {
    uid = id;
    description = fullname;
    isNormalUser = true;
    home = "/home/gaelan";
    group = username;
    createHome = true;
    # Gaelan should have access to network, sound, sudo, video and docker
    extraGroups = [ "users" "networkmanager" "wheel" ];
    # Make sure Gaelan's yubikey can ssh into this machine
    openssh.authorizedKeys.keyFiles = [ ./gaelan-yubikey.pub ];
    # Gaelan uses the Z Shell.
    shell = pkgs.zsh;
  };
}
