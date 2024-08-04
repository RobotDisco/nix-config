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

    home.packages = [
      # Useful debugging tools
      # telnet, traceroute, etc...
      pkgs.inetutils
      # constantly polling a url
      # pkgs.siege
    ];

    # tulip has a custom .ssh/config that has made ... choices.
    # until I need to, just symlink to it for now since we'll likely have that
    # repo cloned.
    home.file.".ssh/config".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/workspace/dotfiles/ssh/config";
  };
}
