{ pkgs, ... }:

{
  imports = [
    ./fileserver.nix
  ];

  config = {
    # For whatever reason I have to configure lightdm to run sessions
    # defined by home-manager's xsession support.
    services.xserver.desktopManager.session = [{
      name = "home-manager";
      start = ''
      ${pkgs.runtimeShell} $HOME/.xsession &
      waitPID = $!
    '';
    }];
    # Since this server is full-disk-encrypted, just automatically log in
    services.xserver.displayManager.autoLogin = {
      enable = true;
      user = "gaelan";
    };

    # Managed home directories + Gaelan's HM customizations.
    home-manager.users.gaelan =
      import ./home-manager.nix;

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
