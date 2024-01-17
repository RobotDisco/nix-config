{ pkgs, ... }:

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
  users.users."gaelan" = { home = "/Users/gaelan"; };

  home-manager.users."gaelan" =
    import ../../home-manager/profiles/gaelan-work.nix;

  nix = {
    # Enable nix flakes
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';

    # Eventually get rid of old nix derivations
    # Let's do weekly on Thursdays during vidq
    gc = {
      automatic = true;
      interval = { Weekday = 5; Hour = 14; Minute = 30; };
      options = "--delete-older-than 30d";
    };
  };

  # Allow Gaelan to set up caches.
  nix.settings.trusted-users = [ "gaelan" ];

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

    casks = [
      "bitwarden"
      "brave-browser"
      "calibre"
      "chef-workstation"
      "discord"
      "kobo"
      "seafile-client"
      "signal"
      "slack"
      "tidal"
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

    masApps = {
      "WireGuard" = 1451685025;
    };
  };

  services.yabai = {
    enable = true;
    enableScriptingAddition = true;

    config = {
      #mouse_follows_focus = "on";
      #focus_follows_mouse = "autofocus";
      layout = "bsp";
      #window_opacity = "on";
      #active_window_opacity = "1.0";
      #normal_window_opacity = "0.80";
      #external_bar = "all:32:0";
    };
  };

  services.spacebar = {
    enable = false;
    package = pkgs.spacebar;

    config = {
      clock_format = "%F%t%R";
      space_icon_strip  = "1 2 3 4 5 6 7 8 9 10";
      text_font = "Verdana:Bold:12.0";
      space_icon_color = "0xfffeff6e";
    };
  };
  services.sketchybar = {
    enable = false;
    config = ''
  ${pkgs.sketchybar}/bin/sketchybar \
    --bar \
    height=32 \
    color=0xffbd00ff \
    --default \
    text.color=0xff3fff2d
    '';
  };
  services.skhd = {
    enable = true;

    skhdConfig = ''
alt - f : ${pkgs.yabai}/bin/yabai -m window --focus east
alt + shift - f : ${pkgs.yabai}/bin/yabai -m window --swap east
alt - b : ${pkgs.yabai}/bin/yabai -m window --focus west
alt + shift - b : ${pkgs.yabai}/bin/yabai -m window --swap west
alt - p : ${pkgs.yabai}/bin/yabai -m window --focus north
alt + shift - p : ${pkgs.yabai}/bin/yabai -m window --swap north
alt - n : ${pkgs.yabai}/bin/yabai -m window --focus south
alt + shift - n : ${pkgs.yabai}/bin/yabai -m window --swap south

alt - return : open -a emacs

alt + shift - 0x18 : ${pkgs.yabai}/bin/yabai -m space --create
alt - 0x1B : ${pkgs.yabai}/bin/yabai -m space --destroy

alt - 0x2B : ${pkgs.yabai}/bin/yabai -m space --focus prev
alt - 0x2F : ${pkgs.yabai}/bin/yabai -m space --focus next

alt + shift - 0x2B : ${pkgs.yabai}/bin/yabai -m window --space prev
alt + shift - 0x2F : ${pkgs.yabai}/bin/yabai -m window --space next

alt + ctrl - 0x12 : ${pkgs.yabai}/bin/yabai -m window --toggle zoom-fullscreen
    '';
  };

  networking.computerName = "Fountain-of-Ahmed-III";
}
