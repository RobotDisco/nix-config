{
  config,
  pkgs,
  robotdisco-secrets,
  ...
}:

let
  # Google demands I use XOAUTH, and this means I have to configure a convoluted
  # SASL thing via some library called gssasl.
  gs_mbsync = pkgs.isync.override {
    withCyrusSaslXoauth2 = true;
  };

  oauth2l = "${pkgs.oauth2l}/bin/oauth2l";
in
{
  age.secrets.google-oauth.file = "${robotdisco-secrets}/google-credentials.json.age";

  accounts.email = {
    accounts.personal = {
      address = "gdcosta@gmail.com";
      flavor = "gmail.com";
      maildir.path = "personal";
      mbsync = {
        enable = true;
        create = "both";
        expunge = "both";
        remove = "both";
        extraConfig.account = {
          AuthMechs = "XOAUTH2";
        };
      };
      mu.enable = true;
      passwordCommand = [
        "${oauth2l} fetch"
        "--credentials ${config.age.secrets.google-oauth.path}"
        "--scope https://mail.google.com"
        # Since I'm authing two different accounts, different cache for each line
        "--cache ~/.oauth2l-personal.cache"
        # Use refresh token if I can.
        "--refresh"
      ];
      primary = true;
      realName = "Gaelan D'costa";
      signature = {
        showSignature = "append";
        text = ":wqwqwq!";
      };
      userName = "gdcosta";
    };
    accounts.work = {
      address = "gaelan@tulip.com";
      aliases = [
        "gaelan@tulip.io"
        "gaelan.dcosta@tulip.com"
      ];
      flavor = "gmail.com";
      maildir.path = "work";
      mbsync = {
        enable = true;
        create = "both";
        expunge = "both";
        remove = "both";
        extraConfig.account = {
          AuthMechs = "XOAUTH2";
        };
      };
      mu.enable = true;
      passwordCommand = [
        "${oauth2l} fetch"
        "--credentials ${config.age.secrets.google-oauth.path}"
        "--scope https://mail.google.com"
        # Since I'm authing two different accounts, different cache for each line
        "--cache ~/.oauth2l-work.cache"
        # Use refresh token if I can.
        "--refresh"
      ];
      realName = "Gaelan D'costa";
      #signature = {};
      userName = "gaelan@tulip.com";
    };

    maildirBasePath = "mail";
  };

  # It's not obvious, but account-specific configuration doesn't actually install
  # the tooling for those programs.
  #
  # If I want these to automatically run eventually, I will have to move them
  # into their respective `services` sections.
  programs = {
    # IMAP mail fetcher
    mbsync = {
      enable = true;
      package = gs_mbsync;

      groups.inboxes = {
        personal = [
          "INBOX"
          "[Gmail]/Starred"
        ];
        work = [
          "INBOX"
          "[Gmail]/Starred"
        ];
      };
    };

    # Mail indexer for local mail processing.
    mu.enable = true;
  };

  home.shellAliases = {
    "mbsa" = "mbsync -a && mu index";
    "mbsi" = "mbsync inboxes && mu index";
  };
}
