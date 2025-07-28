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
  age.secrets.okta-yaml.rekeyFile = "${robotdisco-secrets}/okta-aws-cli.yaml.age";

  home = {
    # In home-manager, the .path attribute relies on the nix config
    # eventually resolving environment variables and command subshells.
    #
    # Apparently using mkOutOfStoreSymlink doesn't work, because it doesn't
    # evaluate the .path attribute including resolutions.
    #
    # So instead, use an activation hook.
    activation.linkOktaYaml = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      run ln -sf $VERBOSE_ARG "${config.age.secrets.okta-yaml.path}" "${config.home.homeDirectory}/.okta/okta.yaml" 
    '';

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
