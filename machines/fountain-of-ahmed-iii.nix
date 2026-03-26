{ robotdisco-secrets, ... }:

{
  imports = [
    ../modules/darwin/nix.nix
    ../secrets/agenix-rekey.nix
  ];

  config = {
    networking.computerName = "Fountain of Ahmed III";
    networking.hostName = "fountain-of-ahmed-iii";

    age.rekey.hostPubkey = "${robotdisco-secrets}/hosts/Fountain-of-Ahmed-III.pub";

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

      # Disable macOS default Spotlight shortcuts (Cmd+Space and
      # Cmd+Option+Space) so aerospace can use those key chords freely.
      # aerospace binds alt-d to open Spotlight instead.
      defaults.CustomUserPreferences."com.apple.symbolichotkeys".AppleSymbolicHotKeys = {
        # Spotlight Search (Cmd+Space)
        "64" = {
          enabled = false;
          value = {
            parameters = [
              32
              49
              1048576
            ];
            type = "standard";
          };
        };
        # Spotlight Window (Cmd+Option+Space)
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
        "yubico-yubikey-manager"
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
