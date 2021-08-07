# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, inputs, ... }:

let

username = "gaelan";
hostName = "arrakis";

in

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ../../profiles/audio.nix
      ../../profiles/cachix.nix
      ../../profiles/common.nix
      ../../profiles/dev.nix
      ../../profiles/fonts.nix
      ../../profiles/gnupg.nix
      ../../profiles/home-hardware.nix
      ../../profiles/keeb-mouse-opinions.nix
      ../../profiles/laptop.nix
      ../../profiles/nix-dev.nix
      ../../profiles/packages.nix
      ../../profiles/regional-settings.nix
      ../../profiles/shell.nix
      ../../profiles/shit-gaelan-likes.nix
      ../../profiles/smb.nix
      ../../profiles/ssd.nix
      ../../profiles/tulip.nix
      ../../profiles/uefi-boot.nix
      ../../profiles/window-manager.nix
      ../../profiles/yubikey.nix
      ../../profiles/zfs.nix
      ../../users/gaelan/default.nix
    ];

  networking.hostName = hostName; # Define your hostname.

  boot.supportedFilesystems = [ "zfs" ];

  networking.hostId = "140f9be5";

  programs.sedutil.enable = true;

  # Self-encrypting drive (OPAL)
  # nixpkgs.config.packageOverrides = pkgs: {
  #  sedutil = (pkgs.sedutil.overrideAttrs (oldAttrs: {
  #     patches = (oldAttrs.patches or []) ++ [
  #       # Add support for enabling unlocking when resuming from sleep
  #       # See: https://github.com/Drive-Trust-Alliance/sedutil/pull/190
  #       (builtins.fetchurl {
  #         url = https://patch-diff.githubusercontent.com/raw/Drive-Trust-Alliance/sedutil/pull/190.patch; 
  #         sha256 = "c0618a319eb0c9a6efe9c72db59338232b235079042ccf77b1d690f64f735a42";
  #       })
  #     ];
  #   }));
  # };

  # NOTE: Generate the password hash with: sudo sedutil-cli --printPasswordHash 'plaintext-password-here' /dev/nvme0n1  
  # systemd.services.sedutil-s3sleep = {
  #   description = "Enable S3 sleep on OPAL self-encrypting drives";
  #   documentation = [ "https://github.com/Drive-Trust-Alliance/sedutil/pull/190" ];
  #   path = [ pkgs.sedutil ];
  #   script = "sedutil-cli -n -x --prepareForS3Sleep 0 ${secrets.opalDiskPasswordHash} /dev/nvme0n1";
  #   wantedBy = [ "multi-user.target" ];
  # };
  # # Sleep
  # #   https://www.kernel.org/doc/html/latest/admin-guide/pm/sleep-states.html#basic-sysfs-interfaces-for-system-suspend-and-hibernation
  # #   Force hybrid-sleep on suspend:
  # #     - When suspending, write RAM to disk (hibernate)
  # #     - When done writing hibernation image, suspend.
  # environment.etc."systemd/sleep.conf".text = pkgs.lib.mkForce "
  #   [Sleep]
  #   SuspendState=disk
  #   SuspendMode=suspend
  # ";

  services.hardware.bolt.enable = true;
}
