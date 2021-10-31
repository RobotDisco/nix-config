{ config, pkgs, ... }:

{
  hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux;
    [ pipewire ];

  programs.steam.enable = true;
}
