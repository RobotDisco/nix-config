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
  options.robot-disco.tulip.kubernetes = {
    enable = lib.mkOption {
      default = cfg.enable;
      description = "Whether to enable Tulip kubernetes tools (for Tulip 7.0)";
      type = types.bool;
    };
  };

  config = lib.mkIf (cfg.enable && cfg.kubernetes.enable) {

    # We need docker here
    robot-disco.tulip.docker.enable = true;

    home.packages = with pkgs; [
      # Core infra
      kubectl
      istioctl

      # Deployment/rollout tooling
      argo-rollouts

      # Infra as code
      terraform-ls
      terraform-lsp
    ];

    # useful shell aliases that are simple enough to apply to all shells
    home.shellAliases = {
      k = "kubectl";
      kar = "kubectl-argo-rollouts";
    };
  };
}
