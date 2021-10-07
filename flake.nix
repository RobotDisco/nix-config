{
  description = "Gaelan's nix-based systems configuration";

  inputs = {
    # Flakes we're going to depend on
    nixpkgs.url = github:nixos/nixpkgs/21.05;
    emacs-overlay.url = github:nix-community/emacs-overlay;
    home-manager.url = github:nix-community/home-manager/release-21.05;
    darwin.url = github:lnl7/nix-darwin/master;
    deploy-rs.url = github:serokell/deploy-rs;

    # Hook up our chosen dependencies to be the ones our other dependencies use
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
  };

  # This is where we define our own stuff
  outputs = { self, nixpkgs, darwin, deploy-rs, emacs-overlay, home-manager,
              ... }:
    let
      common-nixos-modules = [
        ./nixos/profiles/common.nix
        ({ users.mutableUsers = false; })
        ./nixos/users/gaelan.nix
        {
          # I guess the nix overlays form doesn't do anything
          # magic, so I'm overriding it to add overlays
          # within the nixos module system itself?
          nixpkgs.overlays = [
            emacs-overlay.overlay
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
            emacs-overlay.overlay
          ];
        }
      ];
    in

      {
        nixosConfigurations = {
          # The old manual way I ran dockerized services, which I want to replace
          # with either NixOS or Nomad
          salusaold = nixpkgs.lib.nixosSystem
            {
              system = "x86_64-linux";
              modules = common-nixos-modules ++ [
                ./nixos/profiles/kvm-guest.nix
                ./profiles-nixops/traefik.nix
                {
                  networking.hostName = "salusa-old";

                  networking.interfaces.enp1s0.useDHCP = true;
                  # For some reason a second NIC comes up as this
                  networking.interfaces.enp7s0.useDHCP = false;
                  # I only use this for cloud services, so specify the vlan
                  networking.vlans = {
                    vlan50 = { id = 50; interface="enp7s0"; };
                  };
                  # I currently do port forwarding which requires a static IP
                  # TODO is there a way with nomad I can leverage haproxy or something?
                  networking.interfaces.vlan50.ipv4.addresses = [{
                    address = "192.168.50.99";
                    prefixLength = 24;
                  }];
                }
                {
                  users.users.gaelan = {
                    extraGroups = [ "docker" ];
                  };

                  virtualisation.docker.enable = true;

                  #environment.systemPackages = [
                  #  nixpkgs.docker-compose
                  #];
                }
                {
                  fileSystems."/srv/bitwarden" =
                    { device = "chapterhouse.admin.robot-disco.net:/srv/storagepool/data/bitwarden";
                      fsType = "nfs";
                    };
                  fileSystems."/srv/webdav" =
                  { device = "chapterhouse.admin.robot-disco.net:/srv/storagepool/data/webdav";
                      fsType = "nfs";
                  };
                }
              ];
            };
          # My laptop
          arrakis = nixpkgs.lib.nixosSystem
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
		              home-manager.users.gaelan =
      		          import ./home-manager/users/gaelan/default.nix;
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
                networking.firewall.allowedTCPPorts = [ 2049 ];
              }
              {
                networking.hostName = "chapterhouse";
                # Required by ZFS
                networking.hostId = "bff65b11";
                # Slot isn't 1 because of PCI passthru
                #networking.interfaces.enp1s0.useDHCP = false;
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
                  interval = "*-*-01,15 02:00";
                  enable = true;
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
                  defaults.monitored = "-a -o on -s (S/../(05|12|19|26)/./02|L/../(08|22)/./02)";
                };
              }
              {
                # Automatically snapshot ZFS volumes
                services.sanoid = {
                  enable = true;

                  datasets = {
                    "storagepool" = {
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
                  };
                };
              }
            ];
          };
          kaitain = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";

            modules = common-nixos-modules ++ [
              ./nixos/profiles/kvm-guest.nix
              ./nixos/profiles/sendmail.nix
              {
                networking.hostName = "kaitain";
                networking.interfaces.enp1s0.useDHCP = true;
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
          salusaold = {
            fastConnection = true;
            user = "root";
            sshUser = "gaelan";
            hostname = "salusa-old.admin.robot-disco.net";
            profiles.system = {
              path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.salusaold;
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
