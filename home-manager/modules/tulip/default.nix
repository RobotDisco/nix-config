{ config, lib, pkgs, ... }:

let cfg = config.robot-disco.tulip;

in {
  options.robot-disco.tulip = {
    enable = lib.mkEnableOption "Enable Tulip devops environment";
  };

  config = lib.mkIf cfg.enable {
    # Depend on our dev environment
    robot-disco.development-environment = {
      enable = true;
      email = lib.mkDefault "gaelan@tulip.com";
    };

    home.packages = with pkgs; [
      awscli2
      # Docker VM for macs
      colima
      docker

      google-cloud-sdk
    ];
  };
}
