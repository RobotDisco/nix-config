{ ... }:

{
  imports = [
    ./hardware-configuration.nix

    ../../modules/nixos/laptop

    ../../users/gaelan
  ];

  robot-disco.power-management.enableAMD = true;

  networking = {
    hostName = "arrakis";
    # Enable dynamic configuration of primary NIC
    interfaces.wlp1s0.useDHCP = true;
  };

  boot = {
    # Simple password-based Full Disk Encryption
    initrd = {
      luks = {
        devices.nixoscrypt = {
          device = "/dev/nvme0n1p2";

          # I'd rather have TRIM support than perfect security
          allowDiscards = true;
          # increase performance on SSDs
          bypassWorkqueues = true;

          # Use any FIDO device that's been plugged in.
          crypttabExtraOpts = [ "fido2-device=auto" ];

          # Set to false if you need things like networking to happen first
          preLVM = true;
        };
      };
      # Proactively enable systemd as it has better FIDO2+LUKS support.
      systemd.enable = true;
    };

    loader = {
      # Use the systemd-boot EFI boot loader.
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  # Filesystem overrides
  fileSystems =
    let
      btrfs-options = [
        "compress=zstd"
        "noatime"
      ];
    in
    {
      "/".options = btrfs-options;
      "/nix".options = btrfs-options;
      "/home".options = btrfs-options;
      "/boot".options = [ "noatime" ];
    };

  services = {
    # By default btrfs will scrub filesystems multiple times if subvolumes are
    # mounted; explicitly list one subvolume as the others are covered implicitly.
    btrfs.autoScrub.fileSystems = [ "/" ];

    # Support thunderbolt manager software
    hardware.bolt.enable = true;

    # Framework firmware is in the lvfs-testing repo
    fwupd.extraRemotes = [ "lvfs-testing" ];
  };

  # Machine-specific home-manager config: monitor layout and workspace pinning.
  home-manager.sharedModules = [
    {
      # Kanshi manages output configuration when external monitors are attached.
      services.kanshi = {
        enable = true;
        settings = [
          {
            profile = {
              name = "clamshell";
              outputs = [
                {
                  criteria = "eDP-1";
                  status = "disable";
                  mode = "2256x1504";
                  position = "0,0";
                  scale = 1.566667;
                }
                {
                  criteria = "Dell Inc. DELL U2412M M2GCR1CS0T1L";
                  status = "enable";
                  mode = "1920x1200";
                  position = "0,1504";
                }
                {
                  criteria = "Dell Inc. DELL U2412M HT5N364F0GSS";
                  status = "enable";
                  mode = "1920x1200";
                  position = "1920,1504";
                  transform = "270";
                }
              ];
            };
          }
          {
            profile = {
              name = "docked";
              outputs = [
                {
                  criteria = "eDP-1";
                  status = "enable";
                  mode = "2256x1504";
                  position = "0,0";
                  scale = 1.566667;
                }
                {
                  criteria = "Dell Inc. DELL U2412M M2GCR1CS0T1L";
                  status = "enable";
                  mode = "1920x1200";
                  position = "0,1504";
                }
                {
                  criteria = "Dell Inc. DELL U2412M HT5N364F0GSS";
                  status = "enable";
                  mode = "1920x1200";
                  position = "1920,1504";
                  transform = "270";
                }
              ];
            };
          }
          {
            profile = {
              name = "roaming";
              outputs = [
                {
                  criteria = "eDP-1";
                  status = "enable";
                  mode = "2256x1504";
                  position = "0,0";
                  scale = 1.566667;
                }
              ];
            };
          }
        ];
      };

      # Pin named workspaces to physical monitors when docked.
      # Portrait Dell (HT5N364F0GSS) → focus; landscape Dell → everything else.
      # Falls back gracefully to eDP-1 when undocked (roaming).
      wayland.windowManager.sway.extraConfig = ''
        workspace "focus" output "Dell Inc. DELL U2412M HT5N364F0GSS"
        workspace "web" output "Dell Inc. DELL U2412M M2GCR1CS0T1L"
        workspace "comms" output "Dell Inc. DELL U2412M M2GCR1CS0T1L"
        workspace "gaming" output "Dell Inc. DELL U2412M M2GCR1CS0T1L"
      '';
    }
  ];

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "21.11"; # Did you read the comment?
}
