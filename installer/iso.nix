# Inspiration: https://github.com/wagdav/homelab/blob/master/installer
#
# To build the installer for your system architecture:
#
#  nix-build '<nixpkgs/nixos>' -A config.system.build.isoImage \
#    -I nixos-config=iso.nix
#
# For example: To build a 32-bit installer, override the value of the
# `system` parameter:
#
#  nix-build '<nixpkgs/nixos>' -A config.system.build.isoImage \
#     -I nixos-config=iso.nix --argStr system i686-linux
#
#
# NOTE: We assume you have an id_rsa.pub in the default .ssh/
# directory that you will be using to ssh into the system.

{ config, pkgs, system ? builtins.currentSystem, ... }:

{
  imports = [
    # https://nixos.wiki/wiki/Creating_a_NixOS_live_CD
    <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix>

    # Provide an initial copy of the NixOS channel so the user doesn't
    # need to run `nix-channel --update` first.
    <nixpkgs/nixos/modules/installer/cd-dvd/channel.nix>
  ];

  # Enable SSH in the boot process.
  systemd.services.sshd.wantedBy = pkgs.lib.mkForce [ "multi-user.target" ];
  # Make our user immutable so that config will get overriden by the real
  # configuration files that are applied later on.
  users = {
    mutableUsers = false;
    users.root.openssh.authorizedKeys.keyFiles = [ ~/.ssh/id_rsa.pub ];
  };

  # Include some standard commands and config for initial setup in /etc

  environment.etc = {
    "installer-darktower.sh" = {
      source = ./install-darktower.sh;
      mode = "0700";
    };

    "configuration.nix" = {
      source = ./configuration.nix;
      mode = "0600";
    };

    "configuration-darktower.nix" = {
      source = ./configuration-darktower.nix;
      mode = "0600";
    };
  };
}
