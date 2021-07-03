{ config, pkgs, ... }:

{
  home.file.gpg-conf = {
    source = ./gpg.conf;
    target = ".gnupg/gpg.conf";
  };
  home.file.gpg-agent-conf = {
    text = ''
      # https://github.com/drduh/config/blob/master/gpg-agent.conf
      # https://www.gnupg.org/documentation/manuals/gnupg/Agent-Options.html
      allow-emacs-pinentry
      allow-loopback-pinentry
      enable-ssh-support
      default-cache-ttl 60
      max-cache-ttl 120
      ${if pkgs.stdenvNoCC.isLinux then
        "pinentry-program ${pkgs.pinentry}/bin/pinentry-curses"
        else
          "pinentry-program ${pkgs.pinentry_mac}/Applications/pinentry-mac.app/Contents/MacOS/pinentry-mac"}
    '';
    target = ".gnupg/gpg-agent.conf";
  };

  programs.git = {
    enable = true;
    extraConfig = {
      core = {
        autocrlf = "input";
        editor = "emacsclient";
      };
      hub = {
        protocol = "https";
      };
    };
    userEmail = if pkgs.stdenvNoCC.isLinux
                then "gdcosta@gmail.com"
                else "gaelan@tulip.com";
    userName = "Gaelan D'costa";
  };

  programs.ssh = {
    enable = true;
    compression = true;
    controlMaster = "auto";
    forwardAgent = false;

    matchBlocks = {
      "bastion01-tulip-prod" = {
        hostname = "34.192.243.137";
        user = "welladmin";
      };
      "bastion01-s5a-devstaging" = {
        hostname = "34.233.233.36";
        user = "welladmin";
      };
      "*.s5a.dev *.s5a.staging" = {
        user = "welladmin";
        proxyJump = "bastion01-s5a-devstaging";
      };
      "bastion01-s5a-prod" = {
        user = "welladmin";
        hostname = "35.169.186.80";
      };
      "*.s5a.prod" = {
        user = "welladmin";
        proxyJump = "bastion01-s5a-prod";
      };

      "bastion01-michaelkors-devstaging" = {
        user = "welladmin";
        hostname = "34.204.211.250";
      };
      "*.michaelkors.dev *.michaelkors.staging" = {
        user = "welladmin";
        proxyJump = "bastion01-michaelkors-devstaging";
      };
      "bastion01-michaelkors-prod" = {
        user = "welladmin";
        hostname = "34.203.100.128";
      };
      "*.michaelkors.prod" = {
        user = "welladmin";
        proxyJump = "bastion01-michaelkors-prod";
      };

      db02-timekit-prod = {
        hostname="143.110.224.127";
        user = "forge";
      };

      api01-timekit-prod = {
        hostname="104.131.159.91";
        user = "forge";
      };

      api02-timekit-prod = {
        hostname="164.90.247.44";
        user = "forge";
      };

      api01-timekit-staging = {
        hostname="104.236.145.40";
        user = "forge";
      };
    };
  };

  # We need git's config found in a legacy place because of how certain devtools tooling
  # mounts it into dockerized tools.
  home.activation.gitConfigSymlink = config.lib.dag.entryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD ln -sf $VERBOSE_ARG \
    $HOME/.config/git/config $HOME/.gitconfig
  '';

  home.file.emacsSecrets = {
    source = <dotfiles/overlays/20-emacs/emacs/secrets.el>;
    target = ".emacs.d/secrets.el";
  };
  home.file.xmobarConfig = {
    source = <dotfiles/setup/user/xmobarrc>;
    target = ".xmobarrc";
  };
}
