{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.robot-disco.cdrip;
in
{
  options.robot-disco.cdrip = {
    enable = lib.mkEnableOption "Enable CD Ripping functionality";
  };

  config = lib.mkIf cfg.enable {
    home.shellAliases = {
      rip-cd = "${pkgs.cyanrip}/bin/cyanrip -s 6 -D '{album_artist} - {album}' -F '{track} {title}' -L '{album_artist} - {album}' -T os_simple $*";
      rip-multi-cd = "${pkgs.cyanrip}/bin/cyanrip -s 6 -D '{album_artist} - {album}' -F '{disc}-{track} {title}' -L '{album_artist} - {album} (Disc {disc})' -T os_simple $*";
      rip-dvd = "${pkgs.dvdbackup}/bin/dvdbackup -M -i /dev/sr0";
    };
  };
}
