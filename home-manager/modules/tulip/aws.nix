{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) types;
  cfg = config.robot-disco.tulip;
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
    ]
  );
}
