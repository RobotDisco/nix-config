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
      okta-awscli

      (google-cloud-sdk.withExtraComponents (with google-cloud-sdk.components; [gke-gcloud-auth-plugin]))

      # Docker VM for macs
      colima
      docker
      kubectl

      terraform-ls
      terraform-lsp

      # Useful debugging tools
      # telnet, traceroute, etc...
      inetutils
      # constantly polling a url
      siege
    ];

    #okta-awscli config
    home.file.".okta-aws".source =
      # This nix function makes a symlink to a file that is out of the nix
      # store, rather than copying the file into the nix store first.
      #
      # The reason it is used here is because okta-aws wants to write to the
      # file and files in /nix/store are immutable.
      config.lib.file.mkOutOfStoreSymlink
        "${config.home.homeDirectory}/code/nix-config/home-manager/modules/tulip/okta-aws.toml";

    #tulip's .ssh/config
    home.file.".ssh/config".source =
      config.lib.file.mkOutOfStoreSymlink
        "${config.home.homeDirectory}/workspace/dotfiles/ssh/config";
  };
}
