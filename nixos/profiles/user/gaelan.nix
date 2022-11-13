{ config, lib, pkgs, ... }:

let
  cfg = config.robot-disco.user.gaelan;
in

{
  options.robot-disco.user.gaelan = {
    enable = lib.mkEnableOption "Enable the gaelan user.";
    enableExwm = lib.mkEnableOption "Enable EXWM as a window manager.";
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    (lib.mkIf cfg.enableExwm {
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
    })
    {
      # Managed home directories + Gaelan's HM customizations.
      home-manager.users.gaelan = import ../../../home-manager/profiles/gaelan-personal.nix;

      # Gaelan owns this machine, so let him do nix stuff.
      nix.settings.trusted-users = [ "gaelan" ];

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
        openssh.authorizedKeys.keyFiles = [ ./gaelan-yubikey.pub ];
        # Gaelan uses the Z Shell.
        shell = pkgs.zsh;
      };
    }
  ]);
}
