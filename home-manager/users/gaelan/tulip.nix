# Work-specific stuff that probably should live in its own nix
# flake.
{ pkgs, ... }:

let
  python = pkgs.python39Packages;
in

{
  home.packages = with pkgs; [
    # Cloud management
    awscli
    google-cloud-sdk

    # VM config
    chefdk

    # Docker orchestration
    kubectl

    # Work chat tool
    mattermost-desktop
  ] ++ [
    # We'll need virtualenvs for Cloudformation templates
    python.virtualenv
  ];

  # Because we have a bajillion EC2 VMs, each customer in their own VPC,
  # We have to set up a million jump hosts for a million custom domains
  # TODO this is the perfect place for an abstraction
  programs.ssh = {
    matchBlocks = {
      "bastion01-s5a-devstaging" = {
        hostname = "34.233.233.36";
        user = "welladmin";
      };
      "*.s5a.dev *.s5a.staging" = {
        user = "welladmin";
        proxyJump = "bastion01-s5a-devstaging";
      };
      "bastion01-s5a-prod" = {
        user = "welladmin";
        hostname = "35.169.186.80";
      };
      "*.s5a.prod" = {
        user = "welladmin";
        proxyJump = "bastion01-s5a-prod";
      };

      "bastion01-hbc-devstaging" = {
        user = "welladmin";
        hostname = "52.23.191.235";
      };
      "*.hbcgr-bay.dev *.hbcgr-bay.staging *.hbc.dev" = {
        user = "welladmin";
        proxyJump = "bastion01-hbc-devstaging";
      };
      
      "bastion01-hbc-prod" = {
        user = "welladmin";
        hostname = "34.194.98.35";
      };
      "*.hbcgr-bay.prod *.hbcgr-bay.prod *.hbc.prod" = {
        user = "welladmin";
        proxyJump = "bastion01-hbc-prod";
      };
      
      "bastion01-michaelkors-devstaging" = {
        user = "welladmin";
        hostname = "34.204.211.250";
      };
      "*.michaelkors.dev *.michaelkors.staging" = {
        user = "welladmin";
        proxyJump = "bastion01-michaelkors-devstaging";
      };
      "bastion01-michaelkors-prod" = {
        user = "welladmin";
        hostname = "34.203.100.128";
      };
      "*.michaelkors.prod" = {
        user = "welladmin";
        proxyJump = "bastion01-michaelkors-prod";
      };

      "bastion01-well-devstaging" = {
        user = "welladmin";
        hostname = "54.88.81.148";
      };
      "*.well.dev *.well.staging" = {
        user = "welladmin";
        proxyJump = "bastion01-well-devstaging";
      };

      "bastion01-well-prod" = {
        user = "welladmin";
        hostname = "34.194.205.63";
      };
      "*.well.prod" = {
        user = "welladmin";
        proxyJump = "bastion01-well-prod";
      };

      "bastion01-tulip-prod" = {
        user = "welladmin";
        hostname = "34.192.243.137";
      };
      "*.tulip.prod" = {
        user = "welladmin";
        proxyJump = "bastion01-tulip-prod";
      };

      db02-timekit-prod = {
        hostname="143.110.224.127";
        user = "forge";
      };

      api01-timekit-prod = {
        hostname="104.131.159.91";
        user = "forge";
      };

      api02-timekit-prod = {
        hostname="164.90.247.44";
        user = "forge";
      };

      api01-timekit-staging = {
        hostname="104.236.145.40";
        user = "forge";
      };
    };
  };
}
