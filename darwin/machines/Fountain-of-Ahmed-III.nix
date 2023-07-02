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

    taps = [ "homebrew/cask-drivers" ];

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
      "fujitsu-scansnap-manager-s1300"
      "kensingtonworks"
      "uhk-agent"
      "yubico-authenticator"
      "yubico-yubikey-manager"
      "yubico-yubikey-personalization-gui"
    ];

    masApps = {
      "WireGuard" = 1451685025;
    };
  };

  services.yabai = {
    enable = true;
    enableScriptingAddition = true;

    config = {
      mouse_follows_focus = "on";
      # focus_follows_mouse = "autoraise";
      layout = "bsp";
    };

    extraConfig = ''
function setup_space {
  local idx="$1"
  local name="$2"
  local dsply="$3"
  local space=
  echo "setup space $idx: $name"

  space=$(yabai -m query --spaces --space "$idx")
  if [ -z "$space" ]; then
    yabai -m space --create
  fi

  yabai -m space "$idx" --label "$name"
  yabai -m space "$name" --display "$dsply"
}

setup_space 1 main 1
setup_space 2 web 1
setup_space 3 work 2
setup_space 4 emacs 2
setup_space 5 chat 2

yabai -m rule --add app="Brave Browser" space=web
yabai -m rule --add app="emacs" space=emacs
yabai -m rule --add app="Mattermost" space=work
yabai -m rule --add app="Slack" space=chat
yabai -m rule --add app="Signal" space=chat
yabai -m rule --add app="WhatsApp" space=chat
    '';
  };

  services.spacebar = {
    enable = false;
    package = pkgs.spacebar;

    config = {
      display = "main";
      position = "top";
      spaces = "on";
      spaces_for_all_displays = "on";
      clock = "on";
      power = "on";
    };
  };
  services.skhd = {
    enable = true;

    skhdConfig = ''
alt - q : ${pkgs.yabai}/bin/yabai -m window --close
alt - w : ${pkgs.yabai}/bin/yabai -m space --layout bsp
alt - e : ${pkgs.yabai}/bin/yabai -m space --layout float

alt - s : ${pkgs.yabai}/bin/yabai -m space --layout stack
alt - f : ${pkgs.yabai}/bin/yabai -m window --toggle zoom-fullscreen

# shift + alt - f : ${pkgs.yabai}/bin/yabai -m window --toggle native-fullscreen

alt - h : ${pkgs.yabai}/bin/yabai -m window --focus west
alt - j : ${pkgs.yabai}/bin/yabai -m window --focus south
alt - k : ${pkgs.yabai}/bin/yabai -m display --focus next
alt - l : ${pkgs.yabai}/bin/yabai -m window --focus east

shift + alt - h : ${pkgs.yabai}/bin/yabai -m window --warp west
shift + alt - j : ${pkgs.yabai}/bin/yabai -m window --warp south
shift + alt - k : ${pkgs.yabai}/bin/yabai -m window --warp north
shift + alt - l : ${pkgs.yabai}/bin/yabai -m window --warp east

alt - v : ${pkgs.yabai}/bin/yabai -m window --toggle split

alt - 1 : ${pkgs.yabai}/bin/yabai -m space --focus main
alt - 2 : ${pkgs.yabai}/bin/yabai -m space --focus web
alt - 3 : ${pkgs.yabai}/bin/yabai -m space --focus work
alt - 4 : ${pkgs.yabai}/bin/yabai -m space --focus chat

shift + alt - a : ${pkgs.yabai}/bin/yabai -m display --focus prev
shift + alt - s : ${pkgs.yabai}/bin/yabai -m space --focus prev
shift + alt - d : ${pkgs.yabai}/bin/yabai -m space --focus next
shift + alt - f : ${pkgs.yabai}/bin/yabai -m display --focus next
    '';
  };

  networking.computerName = "Fountain-of-Ahmed-III";
}
