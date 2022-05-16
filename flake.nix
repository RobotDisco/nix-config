{
  description = "Gaelan's nix-based systems configuration";

  inputs = {
    # Flakes we're going to depend on
    nixpkgs.url = github:nixos/nixpkgs/nixos-21.11;
    nixos-hardware.url = github:NixOS/nixos-hardware/master;

    home-manager= {
      url = "github:nix-community/home-manager/release-21.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-config = {
      url = github:RobotDisco/emacs-config;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin = {
      url = github:lnl7/nix-darwin/master;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    deploy-rs.url = github:serokell/deploy-rs;
    sops-nix.url = github:Mic92/sops-nix;
  };

  # This is where we define our own stuff
  outputs = { self, nixpkgs, darwin, deploy-rs, emacs-config, home-manager,
              sops-nix, nixos-hardware, ... }:
    let
      gcdUtil = import ./lib {
        inherit (nixpkgs) lib;
        inherit nixpkgs;
      };

      common-nixos-modules = [
        ./nixos/profiles/common.nix
        sops-nix.nixosModules.sops
        { sops.gnupg.sshKeyPaths = [ "/etc/ssh/ssh_host_rsa_key"]; }
        { users.mutableUsers = false; }
        ./nixos/users/gaelan.nix
        ./nixos/users/root.nix
        {
          # I guess the nix overlays form doesn't do anything
          # magic, so I'm overriding it to add overlays
          # within the nixos module system itself?
          nixpkgs.overlays = [
            emacs-config.overlay
          ];
        }
      ];

      common-darwin-modules = [
        ./darwin/profiles/common.nix
        {
          # I guess the nix overlays form doesn't do anything
          # magic, so I'm overriding it to add overlays
          # within the nixos module system itself?
          nixpkgs.overlays = [
            emacs-config.overlay
          ];
        }
      ];

      common-home-manager-modules = [
        emacs-config.homeManagerModules.emacsConfig
      ];
    in

      {
        nixosConfigurations = {
          arrakis2022 = gcdUtil.host.mkHost {
            system = "x86_64-linux";
            hostName = "arrakis";
            dhcpInterfaces = [ "wlp170s0" ];
            kernelPackages = nixpkgs.pkgs.linuxPackages_latest;
            initrdAvailableModules = [ "xhci_pci" "thunderbolt" "nvme" "usb_storage" "usbhid" "sd_mod" ];
            kernelModules = [ "dm-snapshot" ];
            kernelParams = [ "mem_sleep_default=deep" ];
            cpuCores = 8;
            additionModules = [
              nixos-hardware.nixosModules.framework
              {  
                fileSystems."/" =
                  { device = "/dev/disk/by-label/rootpart0";
                    fsType = "ext4";
                    options = [ "noatime" ];
                  };

                fileSystems."/home" =
                  { device = "/dev/disk/by-label/homepart0";
                    fsType = "ext4";
                    options = [ "noatime" ];
                  };

                fileSystems."/nix" =
                  { device = "/dev/disk/by-label/nixpart0";
                    fsType = "ext4";
                    options = [ "noatime" ];
                  };

                fileSystems."/var" =
                  { device = "/dev/disk/by-label/varpart0";
                    fsType = "ext4";
                    options = [ "relatime" ];
                  };

                fileSystems."/boot" =
                  { device = "/dev/disk/by-label/EFIBOOT0";
                    fsType = "vfat";
                  };

                  swapDevices = [ { device = "/dev/disk/by-label/swappart0"; } ];
              }
              {
                powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
                hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
                # high-resolution display
                hardware.video.hidpi.enable = true;
              }
              {
                boot.initrd.kernelModules = [ "vfat" "nls_cp437" "nls_iso8859-1" "usbhid" ];

                boot.initrd.luks.yubikeySupport = true;
                boot.initrd.luks.devices = {
                  cryptdata = {
                    device = "/dev/nvme0n1p2";
                    preLVM = true;
                    allowDiscards = true;
                    bypassWorkqueues = true;
                    yubikey = {
                      slot = 2;
                      twoFactor = true;
                      storage = {
                        device = "/dev/nvme0n1p1";
                      };
                    };
                  };
                };
              }
              {
                ## Use the systemd-boot EFI boot loader.
                boot.loader.systemd-boot.enable = true;
                boot.loader.efi.canTouchEfiVariables = true;
              }
              {
                # Enable network manager for dynamic network configuration
                networking.networkmanager = {
                  enable = true;
                  wifi.powersave = true;
                };
              }
              {
                time.timeZone = "America/Toronto";
                i18n.defaultLocale = "en_CA.UTF-8";
              }
              {
                # Fix font sizes in X
                services.xserver.dpi = 200;

                # Fix sizes of GTK/GNOME ui elements
                environment.variables = {
                  GDK_SCALE = lib.mkDefault "2";
                  GDK_DPI_SCALE = lib.mkDefault "0.5";
                };                
              }
              {
                services.fprintd.enable = true;
              }
              {
                # Disable laptop's touchpad tap-to-click functionality
                services.xserver.libinput.touchpad.tapping = false;
              }
              {
                nix = {
                  # Enable nix flakes
                  package = pkgs.nixFlakes;
                  extraOptions = ''
                      experimental-features = nix-command flakes
                  '';

                  # Enable binary cache downloads of standard nix packages
                  binaryCaches = [
                    "https://nix-community.cachix.org"
                  ];
                  binaryCachePublicKeys = [
                    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
                  ];
                };
              }
            ];
          };

          kaitain = nixpkgs.lib.nixosSystem
            {
              system = "x86_64-linux";
              modules = common-nixos-modules ++ [
                ./nixos/profiles/kvm-guest.nix
                ./nixos/profiles/sendmail.nix
                {
                  networking.hostName = "kaitain";

                  networking.interfaces.enp1s0.useDHCP = true;
                }
                {
                  services.prometheus = {
                    enable = true;

                    scrapeConfigs = [
                      {
                        job_name = "node";
                        static_configs = [
                          {
                            targets = [ "darktower.admin.robot-disco.net:9100" ];
                          }
                          {
                            targets = [ "chapterhouse.admin.robot-disco.net:9100" ];
                          }
                          {
                            targets = [ "kaitain.admin.robot-disco.net:9100" ];
                          }
                          {
                            targets = [ "salusa.admin.robot-disco.net:9100" ];
                          }
                        ];
                      }
                    ];
                  };

                  services.grafana = {
                    enable = true;
                    addr = "0.0.0.0";
                    domain = "kaitain.admin.robot-disco.net";

                    provision.enable = true;
                    provision.datasources = [
                      {
                        name = "Prometheus";
                        url = "http://localhost:9090";
                        type = "prometheus";
                      }
                    ];
                  };

                  networking.firewall.allowedTCPPorts = [ 3000 ];
                }
              ];
            };
          # The old manual way I ran dockerized services, which I want to replace
          # with either NixOS or Nomad
          salusa = nixpkgs.lib.nixosSystem
            {
              system = "x86_64-linux";
              modules = common-nixos-modules ++ [
                ./nixos/profiles/kvm-guest.nix
                ./nixos/profiles/sendmail.nix
                {
                  networking.hostName = "salusa";

                  networking.interfaces.enp1s0.useDHCP = true;
                  # For some reason a second NIC comes up as this
                  networking.interfaces.enp2s0.useDHCP = false;
                  # I only use this for cloud services, so specify the vlan
                  networking.vlans = {
                    vlan50 = { id = 50; interface="enp2s0"; };
                  };
                  # I currently do port forwarding which requires a static IP
                  networking.interfaces.vlan50.ipv4.addresses = [{
                    address = "192.168.50.99";
                    prefixLength = 24;
                  }];
                }
                {
                  networking.firewall.checkReversePath = "loose";
                  networking.firewall.interfaces.enp1s0.allowedTCPPorts = [ 3306 5432 ];
                  networking.firewall.interfaces.vlan50.allowedTCPPorts = [ 80 443 ];

                  sops.secrets.vaultwarden_secrets = {
                    sopsFile = ./secrets/vaultwarden.yaml;
                    format = "yaml";
                  };

                  containers = {
                    reverseproxy = {
                      autoStart = true;
                      config = {
                        security.acme = {
                          acceptTerms = true;
                          email = "gdcosta+letsencrypt@gmail.com";
                        };
                        services.nginx = {
                          enable = true;
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
                            "fallcube.robot-disco.net" = {
                              locations."/" = {
                                proxyPass = "http://localhost:8001";
                              };

                              forceSSL = true;
                              enableACME = true;
                            };
                          };
                        };
                      };
                    };
                    vaultwarden = {
                      autoStart = true;
                      bindMounts."/var/lib/bitwarden_rs" = {
                        hostPath = "/srv/data/vaultwarden";
                        isReadOnly = false;
                      };
                      bindMounts."/run/secrets" = {
                        hostPath = "/run/secrets";
                        isReadOnly = true;
                      };
                      config = {
                        services.bitwarden_rs = {
                          enable = true;
                          dbBackend = "postgresql";
                          environmentFile = "/run/secrets/vaultwarden_secrets";
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
                    databases = {
                      autoStart = true;
                      bindMounts = {
                        "/var/backup/postgresql" = {
                          hostPath = "/srv/backups/postgresql";
                          isReadOnly = false;
                        };
                        "/var/backup/mysql" = {
                          hostPath = "/srv/backups/mariadb";
                          isReadOnly = false;
                        };
                      };

                      config = {
                        services.postgresql = {
                          package = nixpkgs.legacyPackages."x86_64-linux".postgresql_13;
                          enable = true;
                          enableTCPIP = true;
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

                        services.mysql = {
                          enable = true;
                          package = nixpkgs.legacyPackages."x86_64-linux".mariadb;
                        };
                        services.mysqlBackup = {
                          enable = true;
                          databases = [ "ccnet_db" "seafile_db"  "seahub_db" ];
                          calendar = "*-*-* *:05,15,35,45:00";
                          location = "/var/backup/mysql";
                        };
                      };
                    };
                  };
                }
                ./nixos/services/seafile.nix
                {
                  fileSystems = {
                    "/srv/backups" = {
                      device = "chapterhouse.admin.robot-disco.net:/backups";
                      fsType = "nfs";
                    };
                    "/srv/data" = {
                      device = "chapterhouse.admin.robot-disco.net:/data";
                      fsType = "nfs";
                    };
                  };
                }
              ];
            };
          # My laptop
          arrakis2017 = nixpkgs.lib.nixosSystem
            {
              system = "x86_64-linux";
              modules = common-nixos-modules ++ [
                ./nixos/profiles/baremetal.nix
                ./nixos/machines/arrakis
                # The module that loads home-manager
                home-manager.nixosModules.home-manager
                # My anonymous module that has some (probably oughta be common
                # settings and my user's customized home-manager config
                {
                  home-manager.useUserPackages = true;
                  home-manager.useGlobalPkgs = true;
		              home-manager.sharedModules = common-home-manager-modules;
                }
		            {
	                home-manager.users.gaelan = {
		                imports = [
		                  ./home-manager/users/gaelan
		                  ./home-manager/modules/user/gaelan/base.nix
		                ];
                    config = {
                      robotdisco.user.gaelan.base.enable = true;
		                };
                  };
                }
                {
                  fileSystems."/home/gaelan/fileserver" = {
                    device = "chapterhouse.admin.robot-disco.net:/archive";
                    fsType = "nfs";
                    options = [
                      "x-systemd.automount"
                      "user"
                      "noauto"
                      "x-systemd.idle-timeout=600"
                    ];
                  };
                }
              ];
            };
          # My hypervisor
          darktower = nixpkgs.lib.nixosSystem
            {
              system = "x86_64-linux";

              modules = common-nixos-modules ++ [
                ./nixos/profiles/baremetal.nix
                ./nixos/machines/darktower
                ./nixos/profiles/hypervisor.nix
                ./nixos/profiles/hardware/ups.nix
                ./nixos/profiles/sendmail.nix
                {
                  # Since I don't use my pfSense router to set policy for my VMs
                  # it makes more sense to use bridge mode for better performance
                  # especially since a lot of this traffic will be storage traffic
                  networking.macvlans = {
                    admin = {
                      interface = "eno1";
                      mode  = "bridge";
                    };
                    cloud = {
                      interface = "enp6s0f0";
                      mode = "bridge";
                    };
                  };
                }
              ];
            };
          chapterhouse = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";

            modules = common-nixos-modules ++ [
              ./nixos/profiles/kvm-guest.nix
              ./nixos/profiles/sendmail.nix
              {
                services.nfs.server.enable = true;
                networking.firewall.interfaces.enp2s0.allowedTCPPorts = [ 2049 ];
              }
              {
                networking.hostName = "chapterhouse";
                # Required by ZFS
                networking.hostId = "bff65b11";
                # Slot isn't 1 because of PCI passthru
                # Admin VLAN - 192.168.10.0/24
                networking.interfaces.enp2s0.useDHCP = true;
              }
              {
                # Storage configuration for our fileserver
                boot.supportedFilesystems = [ "zfs" ];

                # I don't care about specific mountpoints, so just mount the pools
                boot.zfs.extraPools = [ "storagepool" "backuppool" ];
              }
              {
                # Using interleaved schedule of bimonthly scrubs
                # and long SMART tests (and weekly short SMART tests)
                # found at https://www.truenas.com/community/threads/scrub-and-smart-testing-schedules.20108/      
                # Scrub ZFS pools every bimonthly
                services.zfs.autoScrub = {
                  interval = "*-*-01,15 03:00";
                  enable = true;
                };

                nixpkgs.config.packageOverrides = pkgs: {
                  zfs = nixpkgs.legacyPackages."x86_64-linux".pkgs.zfs.override {
                    enableMail = true;
                  };
                };
                services.zfs.zed = {
                  enableMail = true;
                  settings = {
                    ZED_EMAIL_ADDR = [ "gdcosta@gmail.com" ];
                  };
                };

                # I very much care about the health of my fileserver data
                services.smartd = {
                  enable = true;
                  # I only care about real drives, not system VM drives
                  devices = [
                    {
                      device = "/dev/sda";
                    }
                    {
                      device = "/dev/sdb";
                    }
                    {
                      device = "/dev/sdc";
                    }
                    {
                      device = "/dev/sdd";
                    }
                    {
                      device = "/dev/sde";
                    }
                    {
                      device = "/dev/sdf";
                    }
                    {
                      device = "/dev/sdg";
                    }
                    {
                      device = "/dev/sdh";
                    }
                  ];
                  autodetect = false;
                  notifications = {
                    mail.sender = "root@robot-disco.net";
                    mail.enable = true;
                    mail.recipient = "gdcosta@gmail.com";
                  };
                  # Enable offline tests, schedule long/sort SMART tests as above
                  defaults.monitored = "-a -o on -s (S/../(05|12|19|26)/./03|L/../(08|22)/./03)";
                };
              }
              {
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
              }
              ./nixos/services/borg.nix
              {
                sops.secrets."storagepool_archive_waterworld.key" = {
                  sopsFile = ./secrets/storagepool_archive_waterworld.key;
                  format = "binary";
                };
                sops.secrets."storagepool_archive_documents.key" = {
                  sopsFile = ./secrets/storagepool_archive_documents.key;
                  format = "binary";
                };
              }
            ];
          };
        };

        deploy.nodes = {
          darktower = {
            fastConnection = true;
            user = "root";
            sshUser = "gaelan";
            hostname = "darktower.admin.robot-disco.net";
            profiles.system = {
              path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.darktower;
            };
          };
          chapterhouse = {
            fastConnection = true;
            user = "root";
            sshUser = "gaelan";
            hostname = "chapterhouse.admin.robot-disco.net";
            profiles.system = {
              path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.chapterhouse;
            };
          };
          salusa = {
            fastConnection = true;
            user = "root";
            sshUser = "gaelan";
            hostname = "salusa.admin.robot-disco.net";
            profiles.system = {
              path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.salusa;
            };
          };
          kaitain = {
            fastConnection = true;
            user = "root";
            sshUser = "gaelan";
            hostname = "kaitain.admin.robot-disco.net";
            profiles.system = {
              path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.kaitain;
            };
          };
        };

        checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;

        darwinConfigurations.caladan = darwin.lib.darwinSystem {
          modules = common-darwin-modules ++ [
            home-manager.darwinModules.home-manager
            {
              # Set laptop hostname
              networking.hostName = "caladan";
            }
            {
              # Set up my name and make sure it runs zsh
              users.users."gaelan.dcosta" = {
                description = "Gaelan D'costa";
                shell = nixpkgs.zsh;
              };
            }
            {
              # My anonymous module that has some (probably oughta be common
              # settings and my user's customized home-manager config
              home-manager.useUserPackages = true;
              home-manager.useGlobalPkgs = true;
              home-manager.users."gaelan.dcosta" =
                import ./home-manager/users/gaelan/default.nix;
            }
          ];
        };
      };
}
