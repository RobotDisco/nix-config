# These are handy misc. utilities to have around
{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    file
    fwupd
    mkpasswd
    unzip
  ];
}
