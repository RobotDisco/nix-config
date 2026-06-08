{ ... }:

{
  imports = [
    ../modules/darwin/nix.nix
  ];

  config = {
    networking.computerName = "Fountain of Ahmed III";
    networking.hostName = "fountain-of-ahmed-iii";

    # Used for backwards compatibility, please read the changelog before
    # changing.
    # $ darwin-rebuild changelog
    system.stateVersion = 4;

    # Set up user and home-manager configuration.
    users.users."gaelan" = {
      home = "/Users/gaelan";
    };

    home-manager = {
      users."gaelan" = import ../home-manager/profiles/gaelan-work.nix;

      sharedModules = [
        {
          # Pin AeroSpace workspaces to physical monitors.
          # This machine has two DELL U2412M displays:
          #   (1) = portrait monitor — dedicated focus/Emacs screen
          #   (2) = landscape monitor (primary, has menu bar) — everything else
          programs.aerospace.userSettings."workspace-to-monitor-force-assignment" = {
            focus = "DELL U2412M (1)";
            web = "DELL U2412M (2)";
            comms = "DELL U2412M (2)";
            fun = "DELL U2412M (2)";
          };
        }
      ];
    };

    # Allow Gaelan to set up caches.
    nix.settings.trusted-users = [ "gaelan" ];

    system = {
      keyboard = {
        enableKeyMapping = true;
        remapCapsLockToControl = true;
        # I want this to not affect internal keyboard, but this flag impacts all
        # keyboards.
        # swapLeftCommandAndLeftAlt = true;
      };
      primaryUser = "gaelan";

      # Remap Spotlight to Opt+D (matching sway's $mod+d for
      # app launcher). This must be a system-level shortcut
      # because aerospace can't trigger Spotlight directly.
      # Cmd+Option+Space Spotlight Window is disabled.
      defaults.CustomUserPreferences."com.apple.symbolichotkeys".AppleSymbolicHotKeys = {
        # Input source toggle (C-SPC / C-Opt-SPC) — disabled so Emacs gets C-SPC
        "60" = {
          enabled = false;
          value = {
            parameters = [
              32
              49
              262144
            ];
            type = "standard";
          };
        };
        "61" = {
          enabled = false;
          value = {
            parameters = [
              32
              49
              786432
            ];
            type = "standard";
          };
        };
        # Spotlight Search → Opt+D
        "64" = {
          enabled = true;
          value = {
            parameters = [
              100
              2
              524288
            ];
            type = "standard";
          };
        };
        # Spotlight Window (Cmd+Option+Space) — disabled
        "65" = {
          enabled = false;
          value = {
            parameters = [
              32
              49
              1572864
            ];
            type = "standard";
          };
        };
      };
    };

    programs = {
      # Create /etc/zshrc that loads the nix-darwin environment.
      zsh.enable = true;
    };

    # A lot of packages need to be installed by homebrew; integrate with Nix.
    homebrew = {
      enable = true;

      casks = [
        "bitwarden"
        "calibre"
        "chef-workstation"
        "discord"
        "fujitsu-scansnap-home"
        "kobo"
        "orbstack"
        "seadrive"
        "seafile-client"
        "signal"
        "slack"
        "sunsama"
        "tidal"
        "uhk-agent"
        "vivaldi"
        "whatsapp"
        "yubico-authenticator"
        "zotero"
      ];

      # Upgrade brew packages whenever we run `darwin switch`
      # Note that this makes the operation non-idempotent, but it
      # keeps brew packages up-to-date.
      onActivation = {
        autoUpdate = true;
        upgrade = true;
      };

      masApps = {
        "WireGuard" = 1451685025;
      };
    };
  };
}
