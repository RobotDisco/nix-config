# I mount SMB fileshare sometimes, this requires
# special configuration
{ pkgs, ... }:

let
  cifs-utils = pkgs.cifs-utils;
in

{
  environment.systemPackages = [
    cifs-utils
  ];

  # mount.cifs needs setuid perms if users are allowed
  # to mount SMB volmes themselves. Otherwise all monting
  # will have to be done as root.
  security.wrappers = {
    "mount.cifs".source = "${cifs-utils}/bin/mount.cifs";
  };
}
