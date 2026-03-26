{ pkgs, ... }:

{
  imports = [ ./gaelan-personal.nix ];

  config = {
    home = {
      packages = with pkgs; [
        bitwarden-desktop

        (dwarf-fortress-packages.dwarf-fortress-full.override {
          theme = "cla";
          enableFPS = false;
        })
        nethack
        # flightgear
        # Adventure-game engine
        scummvm
        # Open up GOG artifacts
        innoextract
        # Interactive Fiction engine
        frotz
        # Diablo 1
        devilutionx

        calibre
        # Sunsama is currently a package installed via personal overlay
        sunsama
        zotero

        # Browser
        vivaldi

        # Chat / Messaging
        discord
        signal-desktop
        slack
        wasistlos

      ];
    };

    services.gammastep = {
      enable = true;

      # My Toronto house
      # TODO this should be more easily discoverable/configurable.
      latitude = 43.65972;
      longitude = -79.42929;

      # TODO do I really want this enabled?
      tray = false;
    };

    robot-disco = {
      cdrip.enable = true;
      yubikey.guiSupport = true;
      claude-code.enable = true;

      development-environment = {
        gpgKey = "A517704FBD8D1018!";
        signCommits = true;
      };

      gnupg.enable = true;

      services.seafile-client.enable = true;

      wayland = {
        hyprland.enable = true;
        sway = {
          enable = true;
          lock-screen.enable = true;
        };
      };
    };
  };
}
