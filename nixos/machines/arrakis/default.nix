# My Lenovo X1 Carbon, 5th Generation
{ config, pkgs, lib, inputs, ... }:

let
  hostName = "arrakis";
in

{
  networking.hostName = hostName; # Define your hostname.
  
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      # This is a laptop
      ../../profiles/laptop.nix
    ];

  ## Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  
  ## ZFS-specific configuration
  boot.supportedFilesystems = [ "zfs" ];
  # ZFS requires us to generate a networking Host ID
  networking.hostId = "140f9be5";
  # Since this is an SSD, enable TRIM support
  services.zfs.trim.enable = true;
  # Do the equivalent of an fsck periodically
  services.zfs.autoScrub.enable = true;


  ## Enable some thinkpad-specific kernel modules
  boot.kernelModules = [ "tp_smapi" ];
  boot.extraModulePackages = [ config.boot.kernelPackages.tp_smapi ];
  environment.systemPackages = with pkgs; [
    tpacpi-bat
  ];

  
  hardware = {
    # Lenovos have their famous trackpoint nib; enable
    trackpoint = {
      enable = true;
      emulateWheel = true;
    };
  };

  
  # Support for OPAL self-encrypting drives
  programs.sedutil.enable = true;

  # The Lenovo supports secure thunderbolt, which requires
  # a daemon to manage device authorization
  services.hardware.bolt.enable = true;

  ## The following is old support for having full-disk-encruption
  ## using the lenovo OPAL's built-in encryption. In theory it is
  ## faster than putting a software layer like LUKS on top of it,
  ## but LUKS and ZFS encryption are more streamlined, and they
  ## support Suspend-to-RAM even if it is arguably not secure
  ## enough for the paranoid

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
}
