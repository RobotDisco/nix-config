{
  accounts.email = {
    accounts.personal = {
      address = "gdcosta@gmail.com";
      flavor = "gmail.com";
      maildir.path = "personal";
      mbsync = {
        enable = true;
        create = "maildir";
        expunge = "maildir";
        remove = "maildir";
      };
      mu.enable = true;
      notmuch.enable = true;
      passwordCommand = [
        "rbw"
        "get"
        "google.com"
        "-f"
        "mbsync-app-password"
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
      flavor = "gmail.com";
      maildir.path = "work";
      mbsync = {
        enable = true;
        create = "maildir";
        expunge = "maildir";
        remove = "maildir";
      };
      mu.enable = true;
      notmuch.enable = true;
      passwordCommand = [
        "rbw"
        "get"
        ''"Tulip Okta"''
        "-f"
        "mbsync-app-password"
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
    mbsync.enable = true;
    # Two alternative mail databases for quick searching that I need to evaluate
    # and see which one I prefer.
    mu.enable = true;
    notmuch.enable = true;
  };
}
