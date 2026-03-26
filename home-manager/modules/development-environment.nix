{
  config,
  lib,
  ...
}:

let
  cfg = config.robot-disco.development-environment;

in
{
  options.robot-disco.development-environment = {
    enable = lib.mkEnableOption "Enable nix-centric developer environment";

    signCommits = lib.mkEnableOption "Sign git commits with GPG key";

    gpgKey = lib.mkOption {
      description = "Public key to sign git commits with. Required when signCommits is enabled.";
      type = lib.types.nullOr lib.types.str;
      default = null;
    };
    fullname = lib.mkOption {
      description = "Full name to put in git commits.";
      type = lib.types.str;
    };
    email = lib.mkOption {
      description = "Email address to put in git commits.";
      type = lib.types.str;
    };
    defaultBranch = lib.mkOption {
      description = "Main branch should be named this when creating a new git repo.";
      default = "main";
      type = lib.types.str;
    };
  };

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      {
        assertions = [
          {
            assertion = !cfg.signCommits || cfg.gpgKey != null;
            message = "robot-disco.development-environment.gpgKey must be set when signCommits is enabled";
          }
        ];
      }
      {
        # Let's create a user directory for my helper scripts
        home.sessionPath = [ "~/bin" ];

        home.file."/bin/.keep" = {
          text = "This file intentionally left blank";
        };
      }
      {
        programs.git = {
          enable = true;
          settings = {
            # A lot of these are coming from
            # https://blog.gitbutler.com/how-git-core-devs-configure-git/
            # as my starting point of understanding git settings.
            branch = {
              # Sort by most recent commit date
              sort = "-committerdate";
            };
            column = {
              # Put branch names in a column format to save space
              ui = "auto";
            };
            commit = {
              # Include diff output in the ephemeral part of the commit
              # editing template.
              verbose = "true";
            };
            core = {
              autocrlf = "input";
              # Configure a global gitignore so you don't have to add files
              # over and over.
              excludesfile = "~/.gitignore";
            };
            diff = {
              # Use a better diffing algorithm.
              algorithm = "histogram";
              # Different colour when code is moved
              # treated differently from add/removed
              colorMoved = "plain";
              # Tell me if before/after sections come from index,
              # working tree, etc...
              mnemonicPrefix = true;
              # Visually indicate when files are renamed
              renames = true;
            };
            fetch = {
              # Prune branches locally if the no longer exist remotely.
              prune = true;
              # Prune tags locally if the no longer exist remotely.
              pruneTags = true;
              # Fetch all removes, don't need to fetch them explicitly.
              all = true;
            };
            # Configuration for Emacs' Magit Forge package
            # https://magit.vc/manual/forge.html
            github = {
              user = "RobotDisco";
            };
            gitlab."git.internal.tulip.io" = {
              user = "gaelan";
            };
            help = {
              # If you type in a slightly incorrect command, prompt a revised
              # suggestion.
              autocorrect = "prompt";
            };
            hub = {
              protocol = "https";
            };
            init = {
              # Don't let git whine about default branch names, just set it.
              inherit (cfg) defaultBranch;
            };
            merge = {
              # Show common base code of conflicting commits, YMMV whether this
              # is useful or not.
              conflictstyle = "zdiff3";
            };
            pull = {
              # Always do a rebase when pulling.
              rebase = true;
            };
            push = {
              # Always set up remote branch if it doesn't already exist.
              autoSetupRemote = true;
              # By default, only push current branch to same name on remote.
              default = "simple";
              # Push local tags automatically to server.
              followTags = true;
            };
            rebase = {
              # automatically fold fixup commits into original commits when
              # rebasing.
              # See `git commit --fixup`, really hand.
              # https://blog.gitbutler.com/git-autosquash/
              autoSquash = true;
              # Automatically stash uncomitted files before rebasing.
              autoStash = true;
              # If you have branches of branches, when you do a rebase on a
              # branch, also update any branches downstream from your
              # now-rebased branches to also update derived branches.
              updateRefs = true;
            };
            # REuse REcorded REsolutions
            # This means don't do the same rebase/merge reconciliations over
            # and over again, redo them if they still apply in later commits
            # in the rebase/merge operation.
            rerere = {
              # Store recorded resolutions
              enable = true;
              # Automatically apply them as you go through the list of commits
              # for the merge/rebase operation.
              autoupdate = true;
            };
            tag = {
              # Don't sort lexically, understand dot versions.
              sort = "version:refname";
            };
            user = {
              inherit (cfg) email;
              name = cfg.fullname;
            };
          };
          lfs.enable = true;
        };
      }
      (lib.mkIf cfg.signCommits {
        programs.git = {
          signing = {
            signByDefault = true;
            key = cfg.gpgKey;
          };
        };
      })
      {
        programs = {
          jq.enable = true;
          ripgrep.enable = true;
        };
      }
      {
        programs.direnv = {
          enable = true;
          enableZshIntegration = true;
          nix-direnv = {
            enable = true;
          };
        };
      }
    ]
  );
}
