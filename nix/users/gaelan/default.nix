{ config, pkgs, linux ? false, ... }:

let
  emacsP = pkgs.emacsWithPackagesFromUsePackage {
    config = builtins.readFile ../../../emacs/config.el;
    # By default emacsWithPackagesFromUsePackage will only pull in packages with `:ensure t`.
    # Setting alwaysEnsure to true emulates `use-package-always-ensure` and pulls in all use-package references.
    alwaysEnsure = true;
  };

in

{
  services.emacs.enable = true;
  services.emacs.package = emacsP;

  home-manager.users.gaelan = { config, ... }:
    {
      # Let Home Manager install and manage itself.
      # programs.home-manager.enable = true;

      # This value determines the Home Manager release that your
      # configuration is compatible with. This helps avoid breakage
      # when a new Home Manager release introduces backwards
      # incompatible changes.
      #
      # You can update Home Manager without changing this value. See
      # the Home Manager release notes for a list of state version
      # changes in each release.
      home.stateVersion = "20.03";

      home.extraOutputsToInstall = [ "man" "doc" "info" ];

      home.packages = with pkgs; [
        awscli
        fasd
        google-cloud-sdk
        ripgrep
      ];

      programs.fzf.enable = true;
      programs.fzf.enableZshIntegration = true;

      programs.git.enable = true;
      programs.git.extraConfig = {
        core = {
          autocrlf = "input";
          editor = "emacsclient";
        };
        hub = {
          protocol = "https";
        };
      };
      programs.git.userEmail = if linux
                               then "gdcosta@gmail.com"
                               else "gaelan@tulip.com";
      programs.git.userName = "Gaelan D'costa";

      programs.keychain.enable = true;
      programs.keychain.enableXsessionIntegration = true;
      programs.keychain.enableZshIntegration = true;
      programs.keychain.keys = if linux
                               then [ "id_rsa" "id_rsa.work" ]
                               else [ "id_rsa" ];
      
      programs.ssh.enable = true;
      programs.ssh.compression = true;
      programs.ssh.controlMaster = "auto";
      programs.ssh.forwardAgent = false;

      programs.ssh.matchBlocks = {
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

      programs.zsh.enable = true;
      programs.zsh.enableAutosuggestions = true;
      programs.zsh.enableCompletion = true;
      programs.zsh.autocd = true;
      programs.zsh.defaultKeymap = "emacs";
      programs.zsh.dotDir = ".config/zsh";
      programs.zsh.history = {
        extended = true;
        path = "${config.xdg.dataHome}/zsh/.zsh_history";
      };
      programs.zsh.initExtra = ''
      # initialize fasd, configures completion, hooks, aliases
      command -v fasd >/dev/null 2>&1 && eval "$(fasd --init auto)"

      # If input isn't a command but a directory, switch to it
      setopt auto_cd

      # Treat '#', '~', and '^' as part of patteerns for filename geeneration.
      setopt extended_glob

      # If a glob pattern has no matches, error instead of retaining pattern string
      setopt nomatch

      # cd will implicitly push old directory into directory stack
      setopt auto_pushd
      # Don't push multiple copies of the same directory onto directory stack.
      setopt pushd_ignore_dups

      ## Just use a stock pre-supplied prompt for now.
      autoload -Uz promptinit && promptinit && prompt redhat

      ## Include tdocker dir into PATH
      if [[ -d ~/workspace/dev_scripts/docker/bin ]]; then
          export PATH=$PATH:$HOME/workspace/dev_scripts/docker/bin
      fi

      ### Tulip workflow
      function tclone () {
          mkdir -p ~/workspace/$1 && git clone git@git.internal.tulip.io:$1.git ~/workspace/$1
      }

      ## If vim doesn't exist, invoke vi instead
      command -v vim --help >/dev/null 2>&1 || alias vim=vi
      '';

      programs.zsh.shellAliases = {
        tdl = "tdocker login";
        e = "$VISUAL";
        killemacs="emacsclient -e '(kill-emacs)'";
        pgrep="pgrep -a";
        ls="ls -FGh";
        grep="grep --colour=auto";
        nix-install = "nix-env -f '<nixpkgs>' -iA";
      };

      xsession.enable = if linux then true else false;
      xsession.windowManager.command = "${emacsP}/bin/emacsclient -c";
      xsession.initExtra = "xmobar &";

      # Fix stupid java applications like android studio
      home.sessionVariables._JAVA_AWT_WM_NONREPARENTING = "1";
    };
}
