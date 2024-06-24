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
      okta-aws-cli
      amazon-ecr-credential-helper

      (google-cloud-sdk.withExtraComponents (with google-cloud-sdk.components; [gke-gcloud-auth-plugin]))
      docker-credential-gcr

      # Docker VM for macs
      docker
      istioctl
      kubectl
      argo-rollouts

      terraform-ls
      terraform-lsp

      # Useful debugging tools
      # telnet, traceroute, etc...
      inetutils
      # constantly polling a url
      # siege
    ];

    #tulip's .ssh/config
    home.file.".ssh/config".source =
      config.lib.file.mkOutOfStoreSymlink
        "${config.home.homeDirectory}/workspace/dotfiles/ssh/config";

    # useful shell aliases that are simple enough to apply to all shells
    home.shellAliases = {
      k = "kubectl";
      kar = "kubectl-argo-rollouts";
    };
  };
}
