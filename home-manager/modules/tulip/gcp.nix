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
    (google-cloud-sdk.withExtraComponents (
      with google-cloud-sdk.components; [ gke-gcloud-auth-plugin ]
    ))
    pkgs.docker-credential-gcr
  ];
}
