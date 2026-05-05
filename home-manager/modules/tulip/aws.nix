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

    shellAliases = {
      awscn = "${okta}/bin/okta-aws-cli -p awscn -z";
      awsus = "${okta}/bin/okta-aws-cli -p awsusa -z";
    };
  };
}
