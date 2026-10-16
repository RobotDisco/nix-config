{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.robot-disco.nix-gc;
  olderThan = "30d";
in
{
  options.robot-disco.nix-gc = {
    enable = lib.mkEnableOption "scheduled GC of this user's Nix profiles";
  };

  # The system GC (modules/common/nix.nix) runs as root and never touches
  # profiles under ~/.local/state/nix, so each user collects their own.
  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      {
        nix.gc = {
          automatic = true;
          dates = "weekly";
          options = "--delete-older-than ${olderThan}";
        };
      }

      # On Darwin, home-manager passes nix.gc.options to launchd as a single
      # argv element, which nix-collect-garbage rejects. Split it ourselves.
      # Drop once nix-community/home-manager#7211 is fixed.
      (lib.mkIf pkgs.stdenv.isDarwin {
        launchd.agents.nix-gc.config.ProgramArguments = lib.mkForce [
          "${pkgs.nix}/bin/nix-collect-garbage"
          "--delete-older-than"
          olderThan
        ];
      })
    ]
  );
}
