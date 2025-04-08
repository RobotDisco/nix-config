{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) types;
  cfg = config.robot-disco.tulip;

  okta = pkgs.okta-aws-cli;
in
{
  options.robot-disco.tulip.aws = {
    enable = lib.mkOption {
      default = cfg.enable;
      description = "Whether to enable Tulip AWS tooling";
      type = types.bool;
    };
  };

  config = lib.mkIf (cfg.enable && cfg.aws.enable) (
    lib.mkMerge [
      {
        home.packages = with pkgs; [
          # AWS binaries
          awscli2
          # Integration with our Okta SSO
          okta-aws-cli
        ];
      }
      (lib.mkIf cfg.docker.enable {
        home.packages = [
          # awscli + ECR integration with Docker
          pkgs.amazon-ecr-credential-helper
        ];
      })
      {
        # In home-manager, the .path attribute relies on the nix config
        # eventually resolving environment variables and command subshells.
        #
        # Apparently using mkOutOfStoreSymlink doesn't work, because it doesn't
        # evaluate the .path attribute including resolutions.
        #
        # So instead, use an activation hook.
        home.activation.linkOktaYaml = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          run ln -sf $VERBOSE_ARG "${config.age.secrets.okta-yaml.path}" "${config.home.homeDirectory}/.okta/okta.yaml" 
        '';
      }
      {
        home.shellAliases = {
          awscn = "${okta}/bin/okta-aws-cli -p awscn -z";
          awsus = "${okta}/bin/okta-aws-cli -p awsusa -z";
        };
      }
    ]
  );
}
