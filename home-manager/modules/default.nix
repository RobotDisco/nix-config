{ lib, pkgs, ... }:

{
  imports = [
    ./development-environment.nix
    ./emacs.nix
    ./games.nix
    ./gnupg.nix
    ./tulip
  ];

  config = {
    xdg.enable = true;

    programs.ssh = {
      enable = true;
      compression = true;
      # Don't forward by default, it is insecure
      # Prefer proxyjumping if you can
      forwardAgent = false;
    };

    programs.zsh = {
      # Since I use zsh, make sure home-manager sets it up.
      enable = true;
    };

    home.keyboard = {
      layout = "us";
      options = [ "ctrl:nocaps" ];
    };

    home.packages = lib.mkIf pkgs.stdenv.isLinux (with pkgs; [
      brave
      bitwarden
      calibre
      discord
      seafile-client
      signal-desktop
      slack
    ]);

    services.gammastep = lib.mkIf pkgs.stdenv.isLinux {
      enable = true;

      # My Toronto house
      # TODO this should be more easily discoverable/configurable.
      latitude = 43.659720;
      longitude = -79.429290;

      # TODO do I really want this enabled?
      tray = true;
    };

    pam.yubico.authorizedYubiKeys.ids = [
      # Main Yubikey
      "cccccclbhcbn"
      # Backup Yubikey
      "cccccclbhcbc"
    ];
  };
}
