{
  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  # Create /etc/zshrc that loads the nix-darwin environment.
  programs.zsh.enable = true;

  # Used for backwards compatibility, please read the changelog before
  # changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # Enable non-free software
  nixpkgs.config.allowUnfree = true;

  # Set up user and home-manager configuration.
  users.users."gaelan.dcosta" = { home = "/Users/gaelan.dcosta"; };

  home-manager.users."gaelan.dcosta" =
    import ./home-manager/profiles/gaelan-work.nix;

  # Allow Gaelan to set up caches.
  nix.settings.trusted-users = [ "gaelan.dcosta" ];

  system.keyboard = {
    enableKeyMapping = true;
    remapCapsLockToControl = true;
    # I want this to not affect internal keyboard, but this flag impacts all
    # keyboards.
    # swapLeftCommandAndLeftAlt = true;
  };

  # Alas, home-manager doesn't support gnupg agent via launchd currently.
  programs.gnupg.agent.enable = true;
  programs.gnupg.agent.enableSSHSupport = true;

  # A lot of packages need to be installed by homebrew; integrate with Nix.
  homebrew = {
    enable = true;

    taps = [ "homebrew/cask-drivers" ];

    homebrew.casks = [
      "bitwarden"
      "brave-browser"
      "calibre"
      "chef-workstation"
      "kobo"
      "seafile-client"
      "signal"
      "slack"
      "tidal"
      "whatsapp"
      "zotero"
      # Work already installs/updates these packages
      #"mattermost";
      # Stuff from drivers tap
      "fujitsu-scansnap-manager-s1300"
      "kensingtonworks"
      "uhk-agent"
      "yubico-authenticator"
      "yubico-yubikey-manager"
    ];
  };
}
