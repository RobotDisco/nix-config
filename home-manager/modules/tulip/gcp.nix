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
  options.robot-disco.tulip.gcp = {
    enable = lib.mkOption {
      default = cfg.enable;
      description = "Whether to enable Tulip tooling for Google Cloud.";
      type = types.bool;
    };
  };

  config = lib.mkIf (cfg.enable && cfg.gcp.enable) (
    lib.mkMerge [
      (lib.mkIf cfg.kubernetes.enable {
        home.packages = with pkgs; [
          (google-cloud-sdk.withExtraComponents (
            with google-cloud-sdk.components; [ gke-gcloud-auth-plugin ]
          ))
        ];
      })
      (lib.mkIf (!cfg.kubernetes.enable) { home.packages = [ pkgs.google-cloud-sdk ]; })
      (lib.mkIf cfg.docker.enable { home.packages = [ pkgs.docker-credential-gcr ]; })
    ]
  );
}
