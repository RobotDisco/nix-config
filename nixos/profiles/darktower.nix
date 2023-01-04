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

  users.users.root.initialHashedPassword =
    "$6$rounds=2500000$NC9QlbTMMOJ8$h.coBkWCDI/epZApjonqHPvOjZ4ys8O44OERo2mK5ehB8TUgK8.FWW4tknxXYrlFKa/9t5tGWALBDoUNbCMjx1";
  time.timeZone = "America/Toronto";

  services.openssh.enable = true;

  # networking.firewall.allowedTCPPorts = [ ];
  # networking.firewall.allowedUDPPorts = [ ];

  system.stateVersion = "22.11";

  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot.initrd.availableKernelModules =
    [ "xhci_pci" "ehci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  # My AMT/VNC-enabled server requires this or remote reboots get sad.
  boot.kernelParams = [ "nomodeset" ];

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
    fsType = "vfat";
  };

  swapDevices = [ { label = "swappart0"; } { label = "swappart1"; } ];

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

  hardware.cpu.intel.updateMicrocode =
    lib.mkDefault config.hardware.enableRedistributableFirmware;

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
    zfs = pkgs.zfs.override { enableMail = true; };
  };
  services.zfs.zed = {
    enableMail = true;
    settings = { ZED_EMAIL_ADDR = [ "gdcosta@gmail.com" ]; };
  };

  users.users.gaelan = {
    shell = pkgs.zsh;
    isNormalUser = true;
    home = "/home/gaelan";
    description = "Gaelan D'costa";
    extraGroups = [ "wheel" ];
    # passwordFile = "/run/secrets/users_gaelan_password";
    # temp password just to get me by
    initialHashedPassword =
      "$6$rounds=2500000$cB5yavkAPQdBU$ATYQgQQHsMRQP9kLIIG12MNX62Gb04V.8Pl2.1hMPAN78CpR0qzLYvEuy3sjLw1/eJ90mAKqeSk9eJV.N/e9P0";
  };

  nix.settings.trusted-users = [ "gaelan " ];

  services.sshguard.enable = true;

  networking.defaultGateway = {
    address = "192.168.10.1";
    interface = "eno1";
  };

  networking.nameservers = [ "8.8.8.8" "8.8.4.4" ];

  # I don't care about specific mountpoints, so just mount the pools
  boot.zfs.extraPools = [ "storagepool" "backuppool" ];

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
    localTargetAllow = [
      "change-key"
      "compression"
      "create"
      "destroy"
      "mount"
      "mountpoint"
      "receive"
      "rollback"
    ];
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
    paths = [ "/srv/storagepool/data" "/srv/storagepool/backups" ];
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
    vlan50 = {
      id = 50;
      interface = "enp6s0f1";
    };
  };

  networking.interfaces.vlan50.useDHCP = false;
  # I currently do port forwarding which requires a static IP
  networking.interfaces.vlan50.ipv4.addresses = [{
    address = "192.168.50.99";
    prefixLength = 24;
  }];

  networking.firewall.interfaces.cni-podman0.allowedTCPPorts = [ 3306 11211 ];
  networking.firewall.interfaces.vlan50.allowedTCPPorts = [ 139 80 443 445 ];
  networking.firewall.checkReversePath = "loose";

  containers = {
    fileserver = {
      autoStart = true;
      bindMounts = {
        "/srv/archive" = {
          hostPath = "/srv/storagepool/archive";
          isReadOnly = false;
        };
      };

      config = {
        system.stateVersion = "22.11";

        users.users.gaelan = {
          shell = pkgs.zsh;
          isNormalUser = true;
          home = "/home/gaelan";
          description = "Gaelan D'costa";
          # passwordFile = "/run/secrets/users_gaelan_password";
          # temp password just to get me by
          initialHashedPassword =
            "$6$rounds=2500000$cB5yavkAPQdBU$ATYQgQQHsMRQP9kLIIG12MNX62Gb04V.8Pl2.1hMPAN78CpR0qzLYvEuy3sjLw1/eJ90mAKqeSk9eJV.N/e9P0";
        };

        services.samba = {
          enable = true;
          # Remember to run `smbpasswd -a <user>` to get samba to pick up
          # necessary user passwords
          securityType = "user";

          extraConfig = ''
            workgroup = ROBOT-DISCO
            server string = chapterhouse
            netbios name = chapterhouse
            security = user
            hosts allow = 192.168.20. 127.0.0.1
            hosts deny 0.0.0.0/0
            guest account = nobody
            map to guest = bad user
          '';

          shares = {
            archive = {
              path = "/srv/archive";
              "read only" = "no";
              "guest ok" = "no";
              "create mask" = "0644";
              "directory mask" = "0755";
              "force user" = "gaelan";
              "force group" = "users";
            };
          };
        };
      };
    };

    reverseproxy = {
      autoStart = true;
      config = {
        system.stateVersion = "22.11";

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
              locations."/" = { proxyPass = "http://localhost:8000"; };

              forceSSL = true;
              enableACME = true;
            };
            "fallcube.robot-disco.net" = {
              extraConfig = ''
                proxy_set_header X-Forwarded-For $remote_addr;
              '';

              locations."/" = { proxyPass = "http://localhost:8001"; };

              forceSSL = true;
              enableACME = true;
            };
            "microblog.robot-disco.net" = {
              http2 = true;
              enableACME = true;
              forceSSL = true;

              locations."/" = {
                proxyPass = "http://localhost:4000";
                recommendedProxySettings = false;

                extraConfig = ''
                  etag on;
                  gzip on;

                  add_header 'Access-Control-Allow-Origin' '*' always;
                  add_header 'Access-Control-Allow-Methods' 'POST, PUT, DELETE, GET, PATCH, OPTIONS' always;
                  add_header 'Access-Control-Allow-Headers' 'Authorization, Content-Type, Idempotency-Key' always;
                  add_header 'Access-Control-Expose-Headers' 'Link, X-RateLimit-Reset, X-RateLimit-Limit, X-RateLimit-Remaining, X-Request-Id' always;
                  if ($request_method = OPTIONS) {
                    return 204;
                  }
                  add_header X-XSS-Protection "1; mode=block";
                  add_header X-Permitted-Cross-Domain-Policies none;
                  add_header X-Frame-Options DENY;
                  add_header X-Content-Type-Options nosniff;
                  add_header Referrer-Policy same-origin;
                  add_header X-Download-Options noopen;
                  proxy_http_version 1.1;
                  proxy_set_header Upgrade $http_upgrade;
                  proxy_set_header Connection "upgrade";
                  proxy_set_header Host $host;

                  # NOTE: increase if users need to upload very big files
                  client_max_body_size 16m;
                '';
              };
            };
          };
        };
      };
    };

    microblog = {
      autoStart = true;
      bindMounts = {
        "/var/lib/pleroma" = {
          hostPath = "/srv/storagepool/data/microblog";
          isReadOnly = false;
        };
      };

      config = {
        system.stateVersion = "22.11";

        services.pleroma = {
          enable = true;
          secretConfigFile = "/var/lib/pleroma/secrets.exs";
          configs = [
            ''
            import Config

            config :pleroma, Pleroma.Web.Endpoint,
              url: [host: "microblog.robot-disco.net", scheme: "https", port: 443],
              host: [ip: {127, 0, 0, 1}, port: 4000]

            config :pleroma, :instance,
              name: "More toots about emacs and food",
              email: "gdcosta@gmail.com",
              notify_email: "gdcosta@gmail.com",
              limit: 5000,
              registrations_open: false

            config :pleroma, :frontend_configurations,
              pleroma_fe: %{
                theme: "redmond-xx"
              }

            config :pleroma, :media_proxy,
              enabled: false,
              redirect_on_failure: true

            config :pleroma, Pleroma.Repo,
              adapter: Ecto.Adapters.Postgres,
              username: "pleroma",
              database: "pleroma",
              hostname: "localhost"

            # Configure web push notifications
            config :web_push_encryption, :vapid_details,
              subject: "mailto:gdcosta@gmail.com"

            # ... TO CONTINUE ...
            ''
          ];
        };
      };
    };

    mysql = {
      autoStart = true;
      bindMounts = {
        "/var/backup/mysql" = {
          hostPath = "/srv/storagepool/backups/mariadb";
          isReadOnly = false;
        };
      };
      config = {
        system.stateVersion = "22.11";
        services.mysql = {
          enable = true;
          package = pkgs.mariadb;
        };
        services.mysqlBackup = {
          enable = true;
          databases = [ "ccnet_db" "seafile_db" "seahub_db" ];
          calendar = "*-*-* *:05,15,35,45:00";
          location = "/var/backup/mysql";
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
        system.stateVersion = "22.11";
        services.postgresql = {
          package = pkgs.postgresql_14;
          enable = true;
          enableTCPIP = false;
          authentication = ''
            host vaultwarden vaultwarden samehost scram-sha-256
          '';
          settings.password_encryption = "scram-sha-256";
        };
        services.postgresqlBackup = {
          enable = true;
          location = "/var/backup/postgresql";
          startAt = "*-*-* *:00,15,30,45:00";
        };
        # For upgrading
        # see https://nixos.org/manual/nixos/stable/index.html#module-services-postgres-upgrading
        environment.systemPackages = [
          pkgs.postgresql_14
        ];
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
        system.stateVersion = "22.11";
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

            require_device_email = true;
          };
        };
      };
    };
  };

  virtualisation.podman.extraPackages = [ pkgs.zfs ];

  virtualisation.containers.storage.settings.storage = {
    driver = "zfs";
    graphroot = "/var/lib/containers/storage";
    runroot = "/run/containers/storage";
  };

  virtualisation.oci-containers.containers = {
    "seafile-memcached" = {
      autoStart = true;
      image = "memcached:1.6";
      entrypoint = "memcached";
      cmd = [ "-m" "256" ];
      ports = [ "127.0.0.1:11211:11211" ];
    };
    "seafile-mc" = {
      autoStart = true;
      image = "seafileltd/seafile-mc:9.0.9";
      dependsOn = [ "seafile-memcached" ];
      environmentFiles = [ "/srv/storagepool/data/webdav/seafile_env_vars" ];
      volumes = [ "/srv/storagepool/data/webdav/shared:/shared" ];
      ports = [ "127.0.0.1:8001:8000" ];
    };
  };

  users.users.nut = {
    uid = 84;
    home = "/var/lib/nut";
    createHome = true;
    group = "nut";
    description = "Network UPS Tools service account";
  };
  users.groups."nut" = { gid = 84; };

  power.ups = {
    enable = true;
    mode = "netserver";
    ups = {
      ups = {
        driver = "usbhid-ups";
        port = "auto";
      };
    };
  };

  environment.etc = {
    "nut/upsd.conf" = {
      text = ''
        LISTEN 127.0.0.1
        LISTEN 192.168.10.3
      '';
      user = "nut";
      group = "nut";
      mode = "0600";
    };
    # So you'd think these should be secret, and you'd be right, but I'm forced
    # to use the values from synology so these are effectively public anyway.
    # This does mean I should make sure this doesn't get listend to from
    # public networks.
    "nut/upsd.users" = {
      text = ''
        [monuser]
            password = secret
            upsmon slave
        [monmaster]
            password = secret
            usbmon master
      '';
      user = "nut";
      group = "nut";
      mode = "0600";
    };
    "nut/upsmon.conf" = {
      text = ''
        MONITOR ups@127.0.0.1 1 monmaster secret master
        SHUTDOWNCMD /run/current-system/sw/bin/poweroff
      '';
      user = "nut";
      group = "nut";
      mode = "0600";
    };
  };

  networking.firewall.allowedTCPPorts = [ 3493 ];

  # Get emails for any hard drive failures
  services.smartd = {
    enable = true;
    notifications = {
      mail.sender = "root@robot-disco.net";
      mail.enable = true;
      mail.recipient = "gdcosta@gmail.com";
    };
    # weekly short tests, monthly long tests
    defaults.autodetected = "-a -o on -s (S/../../3/12|L/../01/./17)";
  };

  services.postfix = {
    enable = true;
    rootAlias = "gdcosta@gmail.com";
    relayHost = "out.teksavvy.com";
    relayPort = 587;
  };
}
