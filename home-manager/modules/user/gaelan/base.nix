{ config, lib, pkgs, ... }:

with builtins;
with lib;
let
  cfg = config.robotdisco.user.gaelan.base;
in

{
  options.robotdisco.user.gaelan.base = {
    enable = mkEnableOption "gaelan base profile";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      programs.home-manager.enable = true;

      xdg.enable = true;

      robotdisco = {
        emacsConfig = {
	  enable = true;
	  enableWindowManager = true;
	};
      };
    }
   ]);
}