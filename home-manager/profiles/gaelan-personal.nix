_:

let
  username = "gaelan";
in
{
  home = {
    inherit username;
    homeDirectory = "/home/${username}";
    # The state version is required and should stay at the version you
    # originally installed.
    stateVersion = "22.11";

    # I have particular keyboard preferences
    keyboard = {
      layout = "us";
      options = [ "ctrl:nocaps" ];
    };
  };

  programs = {
    # Let's assume I'm going to be SSHing from any machine I configure.
    # Let's set some basic performance and security defaults.
    ssh = {
      enable = true;
      # Default SSH configs have since been deprecated.
      enableDefaultConfig = false;
      settings."*" = {
        Compression = true;
        # Don't forward by default, it is insecure
        # Prefer proxyjumping if you can
        ForwardAgent = false;
      };
    };

    zsh = {
      # Since I use zsh, make sure home-manager sets it up.
      enable = true;
    };
  };

  robot-disco = {
    yubikey.enable = true;

    # Let's assume I want to be able to use git on any machine I'm on, and commit as myself.
    development-environment = {
      enable = true;
      fullname = "Gaelan D'costa";
      email = "gaelan@fastmail.ca";
      defaultBranch = "trunk";
    };
  };
}
