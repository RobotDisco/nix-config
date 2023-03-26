{ config, lib, pkgs, ... }:

let cfg = config.robot-disco.games;
in {
  options.robot-disco.games = {
    enable = lib.mkEnableOption "Install favourite games";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      (dwarf-fortress-packages.dwarf-fortress-full.override {
        theme = "cla";
        enableFPS = false;
      })
      nethack
      flightgear
      scummvm
    ];
  };
}
