{ config, lib, pkgs, modulesPath, ... }:

{
  networking.hostName = "darktower";
  # Let's lean into ZFS unless I ever need non-ZFS
  boot.supportedFilesystems = [ "zfs" ];
  # This should be parameterized, an eight-character hex string
  networking.hostId = "aa3d3177";
  boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;

  # Don't allow system to change EFI variables
  boot.loader.efi.canTouchEfiVariables = false;
  # Copy kernel files into /boot so /nix/store isn't needed
  boot.loader.generationsDir.copyKernels = true;
  # TL;DR Don't depend on NVRAM state
  boot.loader.grub.efiInstallAsRemovable = true;
  # OpenZFS recommends grub; sure, why not
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # Copy kernels to /boot
  boot.loader.grub.copyKernels = true;
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.zfsSupport = true;
  # Since we mirror our system drives, mirror boot partitions.
  boot.loader.grub.mirroredBoots = [
    {
      devices = [ "nodev" ];
      path = "/boot/efis/EFIBOOT0";
      efiSysMountPoint = "/boot/efis/EFIBOOT0";
    }
    {
      devices = [ "nodev" ];
      path = "/boot/efis/EFIBOOT1";
      efiSysMountPoint = "/boot/efis/EFIBOOT1";
    }
  ];

  # Don't force import of root ZFS pools
  boot.zfs.forceImportRoot = false;
  boot.zfs.forceImportAll = false;

  users.users.root.initialHashedPassword = "$6$rounds=2500000$J3xTIRDh1NBHNJS/$gsXMT1hWNEHseQBGdYKADCNvPOl5xAP/Bb5v0fy8zwsieIS6jPfe9.HvoEKu3Wf8DkVY8/ZHOHRIPxK9unuso1";
  time.timeZone = "America/Toronto";

  services.openssh.enable = true;

  # networking.firewall.allowedTCPPorts = [ ];
  # networking.firewall.allowedUDPPorts = [ ];

  system.stateVersion = "22.05";

  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];
  
  boot.initrd.availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  # My AMT/VNC-enabled server requires this or remote reboots get sad.
  boot.kernelParams = [ "nomodeset"];

  fileSystems."/" = {
    device = "rootpool/nixos/root";
    fsType = "zfs";
    options = [ "zfsutil" "X-mount.mkdir" ];
  };

  fileSystems."/home" = {
    device = "rootpool/nixos/home";
    fsType = "zfs";
    options = [ "zfsutil" "X-mount.mkdir" ];
  };

  # Where containers and possibly VMs live
  fileSystems."/var/lib" = {
    device = "rootpool/nixos/var/lib";
    fsType = "zfs";
    options = [ "zfsutil" "X-mount.mkdir" ];
  };

  # Where logs live
  fileSystems."/var/log" = {
    device = "rootpool/nixos/var/log";
    fsType = "zfs";
    options = [ "zfsutil" "X-mount.mkdir" ];
  };

  fileSystems."/boot" = {
    device = "bootpool/nixos/root";
    fsType = "zfs";
    options = [ "zfsutil" "X-mount.mkdir" ];
  };

  fileSystems."/boot/efis/EFIBOOT0" = {
    device = "/dev/disk/by-label/EFIBOOT0";
    fsType = "vfat";
  };

  fileSystems."/boot/efis/EFIBOOT1" = {
    device = "/dev/disk/by-label/EFIBOOT1";
    fsType= "vfat";
  };

  swapDevices = [
    { label = "swappart0"; }
    { label = "swappart1"; }
  ];

  networking.useDHCP = lib.mkDefault false;
  networking.interfaces.eno1 = {
    useDHCP = lib.mkDefault false;
    ipv4.addresses = [{
      address = "192.168.10.3";
      prefixLength = 24;
    }];
  };
  networking.interfaces.enp6s0f0.useDHCP = lib.mkDefault false;
  networking.interfaces.enp6s0f1.useDHCP = lib.mkDefault false;

  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  services.fstrim.enable = true;
  services.zfs.trim.enable = true;

  # Using interleaved schedule of bimonthly scrubs
  # and long SMART tests (and weekly short SMART tests)
  # found at https://www.truenas.com/community/threads/scrub-and-smart-testing-schedules.20108/      
  # Scrub ZFS pools every bimonthly
  services.zfs.autoScrub = {
    interval = "*-*-01,15 03:00";
    enable = true;
  };

  nixpkgs.config.packageOverrides = pkgs: {
    zfs = pkgs.zfs.override {
      enableMail = true;
    };
  };
  services.zfs.zed = {
    enableMail = true;
    settings = {
      ZED_EMAIL_ADDR = [ "gdcosta@gmail.com" ];
    };
  };

  users.users.gaelan = {
    shell = pkgs.zsh;
    isNormalUser = true;
    home = "/home/gaelan";
    description = "Gaelan D'costa";
    extraGroups = [ "wheel" ];
    # passwordFile = "/run/secrets/users_gaelan_password";
    # temp password just to get me by
    initialHashedPassword = "$6$rounds=2500000$PFL/U0wWeXAEL2j$Xf5r7J6quFZYeHtUlNQwjJIPmlZDwS7bRg5u8yWYq2NGF8WLdyiMbK.n1ymvmR3gbT7nDZ4Pdp/MvbOsyOi0E/";
  };

  nix.trustedUsers = [ "gaelan "];

  services.sshguard.enable = true;

  networking.defaultGateway = {
    address = "192.168.10.1";
    interface = "eno1";
  };

  networking.nameservers = [
    "8.8.8.8"
    "8.8.4.4"
  ];

  # I don't care about specific mountpoints, so just mount the pools
  boot.zfs.extraPools = [ "storagepool" "backuppool"];

  # Automatically snapshot ZFS volumes
  services.sanoid = {
    enable = true;

    datasets = {
      "storagepool/backups" = {
        recursive = true;
        daily = 90;
        hourly = 72;
        monthly = 36;
        autosnap = true;
        autoprune = true;
      };
      "storagepool/data" = {
        recursive = true;
        daily = 90;
        hourly = 72;
        monthly = 36;
        autosnap = true;
        autoprune = true;
      };
    };
  };
  # Automatically replicate data pool to onsite backup
  services.syncoid = {
    enable = true;

    commands = {
      "storagepool/data" = {
        target = "backuppool/storagepool/data";
        recursive = true;
      };
      "storagepool/backups" = {
        target = "backuppool/storagepool/backups";
        recursive = true;
      };
    };
  };

  services.borgbackup.jobs."borgbase" = {
    paths = [
      "/srv/storagepool/data"
      "/srv/storagepool/backups"
    ];
    repo = "mwhkrvt4@mwhkrvt4.repo.borgbase.com:repo";
    encryption = {
      mode = "keyfile-blake2";
      # TODO encode this securely to not manual file placement
      passCommand = "cat /root/keys/borg_encryption_passphrase";
    };
    compression = "zstd";
    startAt = "*-*-* *:05,35:00";
    prune.keep = {
      hourly = 72;
      daily = 90;
      monthly = 36;
    };
  };

  # I only use this for cloud services, so specify the vlan
  networking.vlans = {
    vlan50 = { id = 50; interface="enp6s0f1"; };
  };
  networking.interfaces.vlan50.useDHCP = lib.mkDefault false;
  # I currently do port forwarding which requires a static IP
  networking.interfaces.vlan50.ipv4.addresses = [{
    address = "192.168.50.99";
    prefixLength = 24;
  }];

  networking.firewall.interfaces.vlan50.allowedTCPPorts = [ 80 443 ];
  networking.firewall.checkReversePath = "loose";

  containers = {
    reverseproxy = {
      autoStart = true;
      config = {
        system.stateVersion = "22.05";

        security.acme = {
          acceptTerms = true;
          defaults.email = "gdcosta+letsencrypt@gmail.com";
        };

        services.nginx = {
          enable = true;
          defaultListenAddresses = [ "192.168.50.99" ];
          recommendedOptimisation = true;
          recommendedTlsSettings = true;
          recommendedGzipSettings = true;
          recommendedProxySettings = true;

          virtualHosts = {
            "vaultwarden.robot-disco.net" = {
              locations."/" = {
                proxyPass = "http://localhost:8000";
              };

              forceSSL = true;
              enableACME = true;
            };
            # "fallcube.robot-disco.net" = {
            #   locations."/" = {
            #     proxyPass = "http://localhost:8001";
            #   };

            #   forceSSL = true;
            #   enableACME = true;
            # };
          };
        };
      };
    };

    mysql = {
      autoStart = true;
      bindMounts = {
        "/var/backup/mysql" = {
          hostPath = "/srv/backups/mariadb";
          isReadOnly = false;
        };
      };
      config = {
        system.stateVersion = "21.05";
        services.mysql = {
          enable = true;
          package = pkgs.mariadb;
        };
      };
    };
    
    postgresql = {
      autoStart = true;
      bindMounts = {
        "/var/backup/postgresql" = {
          hostPath = "/srv/storagepool/backups/postgresql";
          isReadOnly = false;
        };
      };
      config = {
        system.stateVersion = "21.05";
        services.postgresql = {
          package = pkgs.postgresql_13;
          enable = true;
          enableTCPIP = false;
          authentication = ''
            host vaultwarden vaultwarden samehost scram-sha-256
          '';
          settings.password_encryption = "scram-sha-256";
        };
       services.postgresqlBackup = {
         enable = false;
         location = "/var/backup/postgresql";
         startAt = "*-*-* *:00,15,30,45:00";
       };
      };
    };
    vaultwarden = {
      autoStart = true;
      bindMounts = {
        "/var/lib/bitwarden_rs" = {
          hostPath = "/srv/storagepool/data/vaultwarden";
          isReadOnly = false;
        };
      };
      config = {
        system.stateVersion = "22.05";
        services.vaultwarden = {
          enable = true;
          dbBackend = "postgresql";
          environmentFile = "/var/lib/bitwarden_rs/vaultwarden_secrets";
          config = {
            signups_allowed = false;
            signups_verify = true;

            domain = "https://vaultwarden.robot-disco.net";
            invitation_org_name = "Robot Disco";

            smtp_host = "out.teksavvy.com";
            smtp_from = "gdcosta@gmail.com";
            smtp_from_name = "Vaultwarden";

            require_device_email=true;
          };
        };        
      };
    };
  };
}
