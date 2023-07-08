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
      focus_follows_mouse = "autofocus";
      layout = "bsp";
      window_origin_display = "focused";
      window_border = "on";
      window_opacity = "on";
      active_window_opacity = "1.0";
      normal_window_opacity = "0.80";
      external_bar = "all";
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
yabai -m rule --add app="Discord" space=chat
yabai -m rule --add app="Slack" space=chat
yabai -m rule --add app="Signal" space=chat
yabai -m rule --add app="WhatsApp" space=chat
    '';
  };

  services.spacebar = {
    enable = true;
    package = pkgs.spacebar;

    config = {
      height = 33;
      clock_format = "%F%t%R";
      space_icon_strip  = "1 2 3 4 5 6 7 8 9 10";
      text_font = "Verdana:Bold:12.0";
      background_color = "0xffbd00ff";
      foreground_color = "0xff3fff2d";
      space_icon_color = "0xfffeff6e";
    };
  };
  services.skhd = {
    enable = true;

    skhdConfig = ''
shift + ralt - q : ${pkgs.yabai}/bin/yabai -m window --close
ralt - w : ${pkgs.yabai}/bin/yabai -m space --layout bsp
ralt - e : ${pkgs.yabai}/bin/yabai -m space --layout float
shift + ralt - e : launchctl stop org.nixos.yabai
shift + ralt - r : launchctl stop org.nixos.yabai; launchctl start org.nixos.yabai 
shift + ralt - y : launchctl start org.nixos.yabai

ralt - a : ${pkgs.yabai}/bin/yabai -m window --focus recent
shift + ralt - a : ${pkgs.yabai}/bin/yabai -m display --focus recent
ralt - s : ${pkgs.yabai}/bin/yabai -m space --layout stack
ralt - f : ${pkgs.yabai}/bin/yabai -m window --toggle zoom-fullscreen
shift + ralt - f : ${pkgs.yabai}/bin/yabai -m window --toggle native-fullscreen

ralt - h : ${pkgs.yabai}/bin/yabai -m window --focus west
ralt - j : ${pkgs.yabai}/bin/yabai -m window --focus south
ralt - k : ${pkgs.yabai}/bin/yabai -m display --focus next
ralt - l : ${pkgs.yabai}/bin/yabai -m window --focus east

shift + ralt - h : ${pkgs.yabai}/bin/yabai -m window --swap west
shift + ralt - j : ${pkgs.yabai}/bin/yabai -m window --swap south
shift + ralt - k : ${pkgs.yabai}/bin/yabai -m window --swap north
shift + ralt - l : ${pkgs.yabai}/bin/yabai -m window --swap east

ralt - v : ${pkgs.yabai}/bin/yabai -m window --toggle split
ralt - b : ${pkgs.yabai}/bin/yabai -m space --balance

ralt - 1 : ${pkgs.yabai}/bin/yabai -m space --focus main
ralt - 2 : ${pkgs.yabai}/bin/yabai -m space --focus web
ralt - 3 : ${pkgs.yabai}/bin/yabai -m space --focus work
ralt - 4 : ${pkgs.yabai}/bin/yabai -m space --focus chat

ralt - 0x2B : ${pkgs.yabai}/bin/yabai -m space --focus prev
ralt - 0x2F : ${pkgs.yabai}/bin/yabai -m space --focus next

shift + ralt - 0x2B : ${pkgs.yabai}/bin/yabai -m space --move prev
shift + ralt - 0x2F : ${pkgs.yabai}/bin/yabai -m space --move next

ralt - 0x21 : ${pkgs.yabai}/bin/yabai -m display --focus prev
ralt - 0x1E : ${pkgs.yabai}/bin/yabai -m display --focus next

shift + ralt - 0x21 : ${pkgs.yabai}/bin/yabai -m display --move prev
shift + ralt - 0x1E : ${pkgs.yabai}/bin/yabai -m display --move next

ralt - 0x18 : ${pkgs.yabai}/bin/yabai -m space --create
ralt - 0x1B : ${pkgs.yabai}/bin/yabai -m space --destroy

ralt - m : ${pkgs.yabai}/bin/yabai -m space --toggle mission-control
    '';
  };

  networking.computerName = "Fountain-of-Ahmed-III";
}
