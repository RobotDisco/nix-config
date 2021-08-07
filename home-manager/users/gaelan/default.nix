# Gaelan's user config
{ pkgs, lib, ... }:

{
  imports = [
    ./apps.nix
    ./emacs
    ./tulip.nix
    ../../profiles/dev.nix
  ];

  home.packages = with pkgs; [
      # A nice default spread of fonts, albeit stolen from Microsoft
      pkgs.corefonts
      # My current fav programming typeface
      pkgs.camingo-code
      # My old fav programming typeface, still used in places
      pkgs.anonymousPro
  ];

  # Enable my favourite fonts.
  fonts.fontconfig.enable = true;

  # Set up some reasonble and secure ssh configuration
  programs.ssh = {
    enable = true;
    compression = true;
    # Don't forward by default, it is insecure
    # Prefer proxyjumping if you can
    forwardAgent = false;
  };

  # install and configure git
  programs.git = {
    enable = true;
    extraConfig = {
      core = {
        autocrlf = "input";
      };
      hub = {
        protocol = "https";
      };
    };

    # TODO write a tulip nix-shell that sets these to tulip email addresses.
    userEmail = "gdcosta@gmail.com";
    userName = "Gaelan D'costa";
  };

  # We need git's config found in a legacy place because of how certain tools
  # mount it into dockerized tools. (axe in particular)
  home.activation.gitConfigSymlink = lib.hm.dag.dagEntryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD ln -sf $VERBOSE_ARG \
    $HOME/.config/git/config $HOME/.gitconfig
  '';

  # Document these
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    # Not in home-manager anymore??
    #enableSyntaxHighlighting = true;
    enableVteIntegration = true;
    autocd = true;
  };

  # Fuzzy finding on the shell
  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };
  
  ## TODO Do I still need to manually write my own "secure" gpg config?

  # home.file.gpg-conf = {
  #   source = ./gpg.conf;
  #   target = ".gnupg/gpg.conf";
  # };
  # home.file.gpg-agent-conf = {
  #   text = ''
  #     # https://github.com/drduh/config/blob/master/gpg-agent.conf
  #     # https://www.gnupg.org/documentation/manuals/gnupg/Agent-Options.html
  #     allow-emacs-pinentry
  #     allow-loopback-pinentry
  #     enable-ssh-support
  #     default-cache-ttl 60
  #     max-cache-ttl 120
  #     ${if pkgs.stdenv.isLinux then
  #       "pinentry-program ${pkgs.pinentry}/bin/pinentry-curses"
  #       else
  #         "pinentry-program ${pkgs.pinentry_mac}/Applications/pinentry-mac.app/Contents/MacOS/pinentry-mac"}
  #   '';
  #   target = ".gnupg/gpg-agent.conf";
  # };
}
