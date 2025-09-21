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
    };

    programs = {
      # Create /etc/zshrc that loads the nix-darwin environment.
      zsh.enable = true;

      # Alas, home-manager doesn't support gnupg agent via launchd currently.
      gnupg.agent = {
        enable = true;
        enableSSHSupport = true;
      };
    };

    # A lot of packages need to be installed by homebrew; integrate with Nix.
    homebrew = {
      enable = true;

      casks = [
        "bitwarden"
        "calibre"
        "chef-workstation"
        "discord"
        "kobo"
        "orbstack"
        "seadrive"
        "seafile-client"
        "signal"
        "slack"
        "sunsama"
        "tidal"
        "vivaldi"
        "whatsapp"
        "zotero"
        # Work already installs/updates these packages
        "mattermost"
        # Stuff from drivers tap
        "fujitsu-scansnap-home"
        "RobotDisco/cask/kensingtonworks"
        "uhk-agent"
        "yubico-authenticator"
        "yubico-yubikey-manager"
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
