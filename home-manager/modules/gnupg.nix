{ config, lib, pkgs, ... }:

let cfg = config.robot-disco.gnupg;

in {
  options.robot-disco.gnupg = { enable = lib.mkEnableOption "Enable GnuPG"; };

  config = lib.mkIf cfg.enable {
    programs.gpg = {
      enable = true;
      # Required on MacOS for GPG to recognise YubiKey.
      # https://github.com/NixOS/nixpkgs/issues/155629
      scdaemonSettings = lib.mkIf pkgs.stdenv.isDarwin {
        disable-ccid = true;
      };
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
      enable = pkgs.stdenv.isLinux;
      enableExtraSocket = true;
      enableSshSupport = true;
      extraConfig = ''
        allow-emacs-pinentry
        allow-loopback-pinentry
      '';
      defaultCacheTtl = 60;
      maxCacheTtl = 120;
    };
  };
}
