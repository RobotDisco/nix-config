{ config, pkgs, ... }:

{
  home.file.gpg-conf = {
    source = ./gpg.conf;
    target = ".gnupg/gpg.conf";
  };
  home.file.gpg-agent-conf = {
    source = ./gpg-agent.conf;
    target = ".gnupg/gpg-agent.conf";
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
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

  # programs.keychain = {
  #   enable = true;
  #   enableXsessionIntegration = true;
  #   enableZshIntegration = true;
  #   agents = ["gpg-agent"];
  # };

  programs.ssh = {
    enable = true;
    compression = true;
    controlMaster = "auto";
    forwardAgent = false;

    matchBlocks = {
      "bastion pfsense cisco" = {
        hostname = "192.168.20.2";
        localForwards = [
          {
            bind.port = 4200;
            host.address = "192.168.10.1";
            host.port = 80;
          }
          {
            bind.port = 4201;
            host.address = "192.168.10.2";
            host.port = 80;
          }
        ];
      };
      jails = {
        hostname = "192.168.10.4";
        proxyJump = "bastion";
      };
      docker = {
        hostname = "192.168.10.50";
        proxyJump = "bastion";
      };
      "bastion01-tulip-prod" = {
        hostname = "34.192.243.137";
        user = "welladmin";
      };
      tulip-servers = {
        host = "*.dev *.staging *.demo *.prod *.internal";
        proxyCommand = "ssh -q bastion01-tulip-prod -- /usr/local/bin/central_ssh.sh %h";
        user = "welladmin";
      };
    };
  };

  # We need git's config found in a legacy place because of how certain devtools tooling
  # mounts it into dockerized tools.
  home.activation.gitConfigSymlink = config.lib.dag.entryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD ln -sf $VERBOSE_ARG \
    $HOME/.config/git/config $HOME/.gitconfig
  '';

  home.file.emacsConfig = {
    source = <dotfiles/overlays/20-emacs/emacs/config.el>;
    target = ".emacs.d/init.el";
  };

  home.file.xmobarConfig = {
    source = <dotfiles/setup/user/xmobarrc>;
    target = ".xmobarrc";
  };
}
    
