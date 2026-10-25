{
  config,
  lib,
  pkgs,
  robotdisco-secrets,
  ...
}:

let
  cfg = config.robot-disco.tulip;

  okta = pkgs.okta-aws-cli;

  # Settings shared by every login. Org domain, OIDC client and AWS federation
  # app come from okta.yaml. okta-aws-cli resolves each setting as: okta.yaml
  # profile > CLI flag > env var > default, so a profile can still override
  # any of these (e.g. a shorter aws-session-duration for admin).
  #
  # Session duration: okta-aws-cli defaults to 1h; the Okta app's session
  # duration only applies to console logins. 43200s = 12h, matching the IAM
  # roles' MaxSessionDuration.
  oktaFlags = lib.concatStringsSep " " [
    "--write-aws-credentials" # -z
    "--expiry-aws-variables" # -x
    "--open-browser" # -b
    "--aws-region us-east-1"
    "--aws-session-duration 43200"
  ];

  oktaWeb = "${okta}/bin/okta-aws-cli web ${oktaFlags}";

  # Profiles defined in okta.yaml (robotdisco-secrets/okta-aws-cli.yaml.age).
  # `pick` has no preset IdP/role, so it always shows the interactive picker.
  profiles = [
    "pick"
    "main-admin"
    "main-audit"
    "it-admin"
    "it-audit"
  ];

  # Log in to a profile, then point this shell's AWS CLI/SDKs at it. Must be
  # an alias (not a script) so the export lands in the calling shell.
  oktaLogin = profile: "${oktaWeb} -p ${profile} && export AWS_PROFILE=${profile}";
in
lib.mkIf cfg.enable {
  age.secrets.okta-yaml = {
    file = "${robotdisco-secrets}/okta-aws-cli.yaml.age";
    path = "${config.home.homeDirectory}/.okta/okta.yaml";
  };

  home = {
    packages = with pkgs; [
      # AWS binaries
      awscli2
      # Integration with our Okta SSO
      okta-aws-cli
      # awscli + ECR integration with Docker
      amazon-ecr-credential-helper
    ];

    # aws-<profile>, e.g. `aws-it-audit`, plus `aws-all` to refresh every role
    # profile (the *-Okta-Okta-auditor ones included) in a single browser login.
    shellAliases = lib.listToAttrs (map (p: lib.nameValuePair "aws-${p}" (oktaLogin p)) profiles) // {
      aws-all = "${oktaWeb} -p pick --all-profiles";
    };
  };
}
