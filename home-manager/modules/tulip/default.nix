{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.robot-disco.tulip;

in
{
  imports = [
    ./aws.nix
    ./docker.nix
    ./gcp.nix
    ./kubernetes.nix
  ];

  options.robot-disco.tulip = {
    enable = lib.mkEnableOption "Enable Tulip cloudplatform environment";
  };

  config = lib.mkIf cfg.enable {
    # Depend on our dev environment
    robot-disco.development-environment = {
      enable = true;
      email = lib.mkDefault "gaelan@tulip.com";
    };

    # tulip has a custom .ssh/config that has made ... choices.
    # until I need to, just symlink to it for now since we'll likely have that
    # repo cloned.
    home = {
      file.".ssh/config".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/workspace/dotfiles/ssh/config";

      packages = [
        # Useful debugging tools
        # telnet, traceroute, etc...
        pkgs.inetutils
        # constantly polling a url
        # pkgs.siege

        (pkgs.writeShellScriptBin "kubesetup" (builtins.readFile ./kubesetup.sh))
      ];

      shellAliases = {
        local-tf = "docker run -it -v $HOME/.aws:/root/.aws -v $PWD:/app -v $(dirname $SSH_AUTH_SOCK):$(dirname $SSH_AUTH_SOCK) -e SSH_AUTH_SOCK=$SSH_AUTH_SOCK -v $HOME/.config/gcloud:/root/.config/gcloud gcr.io/tulip-infra/terraform:0.4.0 bash";
      };
    };
  };
}
