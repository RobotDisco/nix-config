{
  config,
  lib,
  pkgs,
  robotdisco-secrets,
  ...
}:

let
  homeDir = config.home.homeDirectory;
  inherit (config.xdg) configHome;

  # Per-account notmuch config. Each account gets its own DB
  # selected at runtime via NOTMUCH_PROFILE=<account>. Hooks
  # live under ~/.config/notmuch/<account>/hooks so they can be
  # managed declaratively via xdg.configFile.
  #
  # Default mailRoot is ~/mail/<account> (mujmap layout: cur/
  # at the account root). lieer-managed accounts override
  # mailRoot to ~/mail/<account>/mail because gmi init nests
  # the maildir under a `mail/` subdirectory beside its state.
  # The notmuch DB itself stays at ~/mail/<account>/.notmuch
  # in both cases, beside the sync tool's state file.
  mkNotmuchConfig =
    {
      account,
      primaryEmail,
      otherEmails,
      mailRoot ? "${homeDir}/mail/${account}",
    }:
    ''
      [database]
      mail_root=${mailRoot}
      path=${homeDir}/mail/${account}
      hook_dir=${configHome}/notmuch/${account}/hooks

      [user]
      name=Gaelan D'costa
      primary_email=${primaryEmail}
      other_email=${lib.concatStringsSep ";" otherEmails}

      [new]
      tags=
      ignore=/.*[.](json|lock|bak)$/;/.*[.](toml|json|lock)$/

      [search]
      exclude_tags=deleted;spam

      [maildir]
      synchronize_flags=true
    '';

  # Defensive belt-and-suspenders: strip any `new` tag that some
  # other tool might leak in. With [new] tags= empty above, this
  # is a no-op in practice but cheap insurance.
  postNewHook = pkgs.writeShellScript "notmuch-post-new" ''
    ${pkgs.notmuch}/bin/notmuch tag -new -- tag:new || true
  '';

  # `msync <account> [args...]`: per-account mail sync that
  # pins NOTMUCH_PROFILE and cd's to the right maildir before
  # exec'ing the tool that owns that account. Subcommand
  # pattern instead of three separate commands so the
  # interface is intent-named (msync) and the implementation
  # detail (mujmap vs gmi) stays hidden. With no extra args
  # after the account, defaults to `sync`. The bare `msync`
  # invocation prints usage. Mirrors what the lieer systemd
  # units do for auto-sync, so manual and automatic syncs use
  # the same env.
  msyncScript = pkgs.writeShellScript "msync" ''
    set -eu

    usage() {
      cat <<EOF
    Usage: msync <account> [tool args...]

    Sync mail for one account. Defaults to 'sync' if no extra
    args; otherwise passes args through to the underlying
    tool (mujmap for personal, gmi/lieer for work and old).

    Accounts:
      personal   Fastmail account (mujmap)
      work       Work Gmail (lieer)
      old        Old gdcosta@gmail.com archive (lieer)

    Examples:
      msync personal           # sync personal
      msync work               # sync work
      msync old sync           # explicit sync
      msync personal info      # pass 'info' to mujmap
    EOF
    }

    if [ $# -eq 0 ]; then
      usage
      exit 0
    fi

    account="$1"
    shift
    if [ $# -eq 0 ]; then
      set -- sync
    fi

    case "$account" in
      personal)
        cd ${homeDir}/mail/personal
        export NOTMUCH_PROFILE=personal
        exec ${config.programs.mujmap.package}/bin/mujmap "$@"
        ;;
      work)
        cd ${homeDir}/mail/work
        export NOTMUCH_PROFILE=work
        exec ${config.programs.lieer.package}/bin/gmi "$@"
        ;;
      old)
        cd ${homeDir}/mail/personal-old
        export NOTMUCH_PROFILE=personal-old
        exec ${config.programs.lieer.package}/bin/gmi "$@"
        ;;
      -h|--help|help)
        usage
        exit 0
        ;;
      *)
        echo "msync: unknown account '$account'" >&2
        echo >&2
        usage >&2
        exit 2
        ;;
    esac
  '';

  msyncCompletion = pkgs.writeText "_msync" ''
    #compdef msync

    _msync() {
      local context state line
      _arguments \
        '1: :->account' \
        '*::tool-arg:'

      case $state in
        account)
          _values 'mail account' \
            'personal[Fastmail account, mujmap-synced]' \
            'work[Work Gmail, lieer-synced]' \
            'old[Old gdcosta@gmail.com, lieer-synced]' \
            'help[Show usage]'
          ;;
      esac
    }

    _msync "$@"
  '';

  msync = pkgs.stdenvNoCC.mkDerivation {
    name = "msync";
    dontUnpack = true;
    nativeBuildInputs = [ pkgs.installShellFiles ];
    installPhase = ''
      install -Dm755 ${msyncScript} $out/bin/msync
      installShellCompletion --zsh --name _msync ${msyncCompletion}
    '';
  };
in
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

    # We roll three per-account notmuch profiles below instead
    # of the single shared DB this option would otherwise
    # produce. Each account's bidirectional sync tool
    # (mujmap/lieer) needs its own tag namespace; sharing one
    # notmuch DB across all three caused mujmap to attempt
    # creating Fastmail mailboxes named after work Gmail labels
    # and hit JMAP 400s. See xdg.configFile entries below.
    notmuch.enable = false;
  };

  xdg.configFile = {
    "notmuch/personal/config".text = mkNotmuchConfig {
      account = "personal";
      primaryEmail = "gaelan@fastmail.ca";
      otherEmails = [
        "gaelan.dcosta@fastmail.com"
        "gaelan@robot-disco.net"
      ];
    };
    "notmuch/work/config".text = mkNotmuchConfig {
      account = "work";
      primaryEmail = "gaelan@tulip.com";
      otherEmails = [
        "gaelan@tulip.io"
        "gaelan.dcosta@tulip.com"
      ];
      mailRoot = "${homeDir}/mail/work/mail";
    };
    "notmuch/personal-old/config".text = mkNotmuchConfig {
      account = "personal-old";
      primaryEmail = "gdcosta@gmail.com";
      otherEmails = [ ];
      mailRoot = "${homeDir}/mail/personal-old/mail";
    };

    "notmuch/personal/hooks/post-new" = {
      source = postNewHook;
      executable = true;
    };
    "notmuch/work/hooks/post-new" = {
      source = postNewHook;
      executable = true;
    };
    "notmuch/personal-old/hooks/post-new" = {
      source = postNewHook;
      executable = true;
    };
  };

  home.packages = [
    # notmuch is normally installed via programs.notmuch.enable,
    # but we set that to false above to take over config
    # generation. Install the binary directly so notmuch / notmuch
    # new / etc. remain on $PATH for interactive use.
    pkgs.notmuch
    # `msync <account>` wrapper. See the let-block above for the
    # full script + zsh completion. The home-manager-generated
    # lieer systemd units (when services.lieer.enable +
    # per-account sync.enable are true) do equivalent env setup
    # for auto-sync; this wrapper is for manual invocation that
    # needs to mirror it.
    msync
  ];
  # Auto-sync, gated to Linux: home-manager's services.lieer is a
  # systemd service module and refuses non-Linux platforms.
  # Fastmail/mujmap has no home-manager service module — run manually.
  services = lib.mkIf pkgs.stdenv.isLinux {
    # Gmail
    lieer.enable = true;
  };
}
