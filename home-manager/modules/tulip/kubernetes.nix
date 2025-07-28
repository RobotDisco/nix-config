{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.robot-disco.tulip;
in
lib.mkIf cfg.enable {
  home.packages = with pkgs; [
    # Core infra
    kubectl
    istioctl

    # Nice tool for viewing kubectl requests/limits
    kube-capacity

    # Deployment/rollout tooling
    argocd
    argo-rollouts
    kubernetes-helm

    # Infra as code
    terraform-ls
    terraform-lsp
  ];

  # useful shell aliases that are simple enough to apply to all shells
  home.shellAliases = {
    k = "kubectl";
    kar = "kubectl-argo-rollouts";
  };
}
