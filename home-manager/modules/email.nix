{
  config,
  lib,
  pkgs,
  robotdisco-secrets,
  ...
}:

{
  age.secrets = {
    fastmail-mujmap-pass.file = "${robotdisco-secrets}/fastmail-mujmap-pass.age";
    google-oauth.file = "${robotdisco-secrets}/google-credentials.json.age";
  };
  accounts.email = {
    accounts = {
      personal = {
        primary = true;
        address = "gaelan@fastmail.ca";
        aliases = [
          "gaelan.dcosta@fastmail.com"
          "gaelan@robot-disco.net"
        ];
        flavor = "fastmail.com";
        maildir.path = "personal";
        mu.enable = true;
        notmuch.enable = true;
        mujmap = {
          enable = true;
          settings = {
            username = "gaelan@fastmail.ca";
            password_command = "cat ${config.age.secrets.fastmail-mujmap-pass.path}";
          };
        };
        realName = "Gaelan D'costa";
        signature = {
          showSignature = "append";
          text = ":wqwqwq!";
        };
      };
      # Frozen archive of the old gdcosta@gmail.com account: indexed
      # locally by mu/notmuch but no longer synced. New mail goes to
      # Fastmail.
      personal-old = {
        address = "gdcosta@gmail.com";
        flavor = "gmail.com";
        lieer = {
          enable = true;
          sync.enable = false;
        };
        maildir.path = "personal-old";
        mu.enable = true;
        notmuch.enable = true;
        realName = "Gaelan D'costa";
        signature = {
          showSignature = "append";
          text = ":wqwqwq!";
        };
        userName = "gdcosta";
      };
      work = {
        address = "gaelan@tulip.com";
        aliases = [
          "gaelan@tulip.io"
          "gaelan.dcosta@tulip.com"
        ];
        flavor = "gmail.com";
        lieer = {
          enable = true;
          sync.enable = false;
        };
        maildir.path = "work";
        mu.enable = true;
        notmuch.enable = true;
        realName = "Gaelan D'costa";
        #signature = {};
        userName = "gaelan@tulip.com";
      };
    };

    maildirBasePath = "mail";
  };

  # It's not obvious, but account-specific configuration doesn't actually install
  # the tooling for those programs.
  #
  # If I want these to automatically run eventually, I will have to move them
  # into their respective `services` sections.
  programs = {
    # Install fetcher binaries; the services block below decides
    # which ones auto-sync.
    ## Gmail
    lieer.enable = true;
    ## Fastmail
    # Override the broken upstream mujmap in nixpkgs (v0.2.0) with
    # our pinned Lyndeno fork. Done here rather than via overlay so
    # the override stays scoped to its only consumer.
    mujmap = {
      enable = true;
      package = pkgs.callPackage ../../packages/mujmap.nix { };
    };

    # Mail indexers I'm trying out for local mail processing.
    mu.enable = true;
    notmuch = {
      enable = true;

      new.tags = [ "new" ];
      maildir.synchronizeFlags = true;
      search.excludeTags = [
        "deleted"
        "spam"
      ];
    };
  };
  # Auto-sync, gated to Linux: home-manager's services.lieer is a
  # systemd service module and refuses non-Linux platforms.
  # Fastmail/mujmap has no home-manager service module — run manually.
  services = lib.mkIf pkgs.stdenv.isLinux {
    # Gmail
    lieer.enable = true;
  };
}
