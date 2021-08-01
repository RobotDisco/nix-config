{ pkgs, lib, ... }:

let
  dag = import ../../ext/dag.nix { inherit lib; };
  
  emacsEnv = pkgs.emacsWithPackagesFromUsePackage {
    config = ./emacs/init.org;
  };
  # init-el = pkgs.emacs.trivialBuild {
  #   pname = "init-el";
  #   src = lib.sourceByRegex ./emacs [ "*.org" ];

  #   preBuild = ''
  #     # Tangle org files
  #     emacs --batch -Q \
  #       -l org \
  #       *.org \
  #       -f org-babel-tangle

  #     # Fake config directory in order to have files on load-path
  #     mkdir -p .xdg-config
  #     ln -s $PWD .xdg-config/emacs
  #     export XDG_CONFIG_HOME="$PWD/.xdg-config"

  #     emacs --batch -Q \
  #       -l package \
  #       -eval '(setq package-quickstart t)' \
  #       -f package-quickstart-refresh
  #   '';
  # };

in

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
      ${if pkgs.stdenv.isLinux then
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
    userEmail = if pkgs.stdenv.isLinux
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

      "bastion01-hbc-devstaging" = {
        user = "welladmin";
        hostname = "52.23.191.235";
      };
      "*.hbcgr-bay.dev *.hbcgr-bay.staging *.hbc.dev" = {
        user = "welladmin";
        proxyJump = "bastion01-hbc-devstaging";
      };
      
      "bastion01-hbc-prod" = {
        user = "welladmin";
        hostname = "34.194.98.35";
      };
      "*.hbcgr-bay.prod *.hbcgr-bay.prod *.hbc.prod" = {
        user = "welladmin";
        proxyJump = "bastion01-hbc-prod";
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

      "bastion01-well-devstaging" = {
        user = "welladmin";
        hostname = "54.88.81.148";
      };
      "*.well.dev *.well.staging" = {
        user = "welladmin";
        proxyJump = "bastion01-well-devstaging";
      };

      "bastion01-tulip-prod" = {
        user = "welladmin";
        hostname = "34.192.243.137";
      };
      "*.tulip.prod" = {
        user = "welladmin";
        proxyJump = "bastion01-tulip-prod";
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
  home.activation.gitConfigSymlink = dag.dagEntryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD ln -sf $VERBOSE_ARG \
    $HOME/.config/git/config $HOME/.gitconfig
  '';

  home.file.emacsConfig = {
    #sourceFile = "${init-el}/.xdg-config/emacs/init.el";
    source = ./emacs/init.el;
    target = ".emacs.d/init.el";
  };
  home.file.emacsPomodoroStartSound = {
    source = ./emacs/audio/incoming_hail2.mp3;
    target = ".emacs.d/audio/incoming_hail2.mp3";
  };
  home.file.emacsPomodoroFinishSound = {
    source = ./emacs/audio/ds9intercom.mp3;
    target = ".emacs.d/audio/ds9intercom.mp3";
  };
  home.file.emacsPomodoroFinishLongSound = {
    source = ./emacs/audio/computerbeepsequence1.mp3;
    target = ".emacs.d/audio/computerbeepsequence1.mp3";
  };
  home.file.emacsSecrets = {
    source = ./emacs/secrets.el;
    target = ".emacs.d/secrets.el";
  };
    home.file.stalonetrayConfig = {
    source = ./stalonetrayrc;
    target = ".stalonetrayrc";
  };
  home.file.xmobarConfig = {
    source = ./xmobarrc;
    target = ".xmobarrc";
  };
}
