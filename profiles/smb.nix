# I mount SMB fileshare sometimes
{ pkgs, ... }:

let
  cifs-utils = pkgs.cifs-utils;
in

{
  environment.systemPackages = [
    cifs-utils
  ];

  security.wrappers = {
    "mount.cifs".source = "${cifs-utils}/bin/mount.cifs";
  };
}
