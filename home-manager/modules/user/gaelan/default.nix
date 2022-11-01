# Gaelan's user configuration
{ config, pkgs, lib, ... }:
let cfg = config.robot-disco.user.gaelan;
in {
  options.robot-disco.user.gaelan = {
    enable = lib.mkEnableOption "Enable gaelan user config";
  };

  config = lib.mkIf cfg.enable {
    xdg.enable = true;

    robot-disco.emacs = {
      enable = true;

      enableServer = true;
      enableExwm = true;
    };

    programs.home-manager.enable = true;

    programs.ssh = {
      enable = true;
      compression = true;
      # Don't forward by default, it is insecure
      # Prefer proxyjumping if you can
      forwardAgent = false;
    };

    programs.zsh = {
      # Since I use zsh, make sure home-manager sets it up.
      enable = true;
    };

    # install and configure git
    programs.git = {
      enable = true;
      extraConfig = {
        core = { autocrlf = "input"; };
        hub = { protocol = "https"; };
      };

      # TODO How do I get work laptop pointing to work email?
      userEmail = "gdcosta@gmail.com";
      userName = "Gaelan D'costa";
    };

    programs.gpg = {
      enable = true;
      settings = {
        personal-cipher-preferences = "AES256 AES192 AES";
        personal-digest-preferences = "SHA512 SHA384 SHA256";
        personal-compress-preferences = "ZLIB BZIP2 ZIP Uncompressed";
        default-preference-list =
          "SHA512 SHA384 SHA256 AES256 AES192 AES ZLIB BZIP2 ZIP Uncompressed";
        cert-digest-algo = "SHA512";
        s2k-digest-algo = "SHA512";
        s2k-cipher-algo = "AES256";
        charset = "utf-8";
        fixed-list-mode = true;
        no-comments = true;
        no-emit-version = true;
        no-greeting = true;
        keyid-format = "0xlong";
        list-options = "show-uid-validity";
        verify-options = "show-uid-validity";
        with-fingerprint = true;
        require-cross-certification = true;
        no-symkey-cache = true;
        use-agent = true;
        throw-keyids = true;
      };
    };

    services.gpg-agent = {
      enable = true;
      enableExtraSocket = true;
      enableSshSupport = true;
      extraConfig = ''
        allow-emacs-pinentry
        allow-loopback-pinentry
      '';
      defaultCacheTtl = 60;
      maxCacheTtl = 120;
    };

    # Temporarily turn off but should only be on for laptop anyway 
    services.screen-locker = {
      enable = true;
      lockCmd = "${pkgs.i3lock}/bin/i3lock -c 746542";
    };

    home.keyboard = {
      layout = "us";
      options = [ "ctrl:nocaps" ];
    };

    home.packages = with pkgs; [
      brave
      bitwarden
      networkmanagerapplet
      seafile-client
      slack

      # This should probably be part of an exwm module
      pinentry-emacs

      # These are part of my developer portfolio
      jq
      ripgrep

      # Games worth playing
      (dwarf-fortress-packages.dwarf-fortress-full.override {
        theme = "cla";
        enableFPS = false;
      })
      nethack
    ];

    programs.direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv = {
        enable = true;
      };
    };
  };
}
