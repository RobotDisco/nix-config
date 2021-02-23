# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, ... }:

{
  imports =
    [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" "vfio-pci" ];
  boot.extraModulePackages = [ ];
  # modeset screws up Intel AMT Remote Contrl for some reason
  boot.kernelParams = [ "intel_iommu=on nomodeset" ];

  # handle case where too many hardlinks in nix store for ZFS.
  boot.loader.grub.copyKernels = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Enable ZFS for booting
  boot.supportedFilesystems = [ "zfs" ];

  # ZFS requires a networking hostID
  networking.hostId = "526b897e";

  # Manually configure networking, which I _think_ is cattle configuration
  networking.interfaces.br-admin.ipv4 = {
    addresses = [ { address = "192.168.10.3"; prefixLength = 24; } ];
  };

  networking.nameservers = [ "192.168.10.1"];
  networking.defaultGateway = {
    address = "192.168.10.1";
    interface = "br-admin";
  };

  networking.bridges = {
    br-admin = {
      interfaces = [ "eno1" ];
      rstp = true;
    };
    br-trunk = {
      interfaces = [ "enp5s0f0" ];
      rstp = true;
    };
  };
  
  fileSystems."/" =
    { device = "rootpool/safe/ROOT/nixos";
      fsType = "zfs";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-label/EFIBOOT0";
      fsType = "vfat";
    };

  fileSystems."/home" =
    { device = "rootpool/safe/home";
      fsType = "zfs";
    };

  fileSystems."/nix" =
    { device = "rootpool/local/nix";
      fsType = "zfs";
    };

  fileSystems."/var" =
    { device = "rootpool/safe/var";
      fsType = "zfs";
    };

  boot.zfs.extraPools = [ "vmpool" "salusajail" "backuppool" ];

  swapDevices =
    [ { device = "/dev/disk/by-label/swappart"; }
    ];

  nix.maxJobs = lib.mkDefault 4;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  services.nfs.server.enable = true;
  services.nfs.server.exports = ''
    /salusajail/data/webdav salusa0.admin.robot-disco.net(rw,no_root_squash)
    /salusajail/data/bitwarden salusa0.admin.robot-disco.net(rw,no_root_squash)
    /salusajail/data/minecraft salusa1.admin.robot-disco.net(rw,no_root_squash)
  '';
  networking.firewall.enable = false;

  services.samba = {
    enable = true;
    syncPasswordsByPam = true;

    extraConfig = ''
     workgroup=ROBOT-DISCO
     server string = fileserver
     netbios name = fileserver
#     hosts allow = 192.168.20 localhost
#     hosts deny = 0.0.0.0/0
     guest account = nobody
     map to guest = bad user
    '';
    
    shares = {
      fileserver = {
        path = "/salusajail/data/fileserver";
        browseable = "yes";
        comment = "Gaelan's fileserver";
        "read only" = "no";
        "create mask" = "0644";
        "directory mask" = "0755";
        "force user" = "gaelan";
        "force group" = "users";
      };
    };
  };
  networking.firewall.allowedTCPPorts = [ 445 139 ];
  networking.firewall.allowedUDPPorts = [ 137 138 ];
  
  services.sanoid = {
    enable = true;

    datasets = {
      "salusajail/data" = {
        recursive = true;
        daily = 90;
        hourly = 72;
        monthly = 36;
        autosnap = true;
        autoprune = true;
      };
    };
  };

  services.syncoid = {
    enable = true;

    commands = {
      "salusajail/data" = {
        target = "backuppool/salusajail/data";
        recursive = true;
      };
    };
  };
}
