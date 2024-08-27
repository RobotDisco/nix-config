{ pkgs, ... }:

{
  networking.firewall.interfaces.vlan50.allowedTCPPorts = [
    139
    445
  ];

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
        system.stateVersion = "22.05";

        programs.zsh.enable = true;

        users.users.gaelan = {
          shell = pkgs.zsh;
          isNormalUser = true;
          home = "/home/gaelan";
          description = "Gaelan D'costa";
          # hashPasswordFile = "/run/secrets/users_gaelan_password";
          # temp password just to get me by
          initialHashedPassword = "$6$rounds=2500000$cB5yavkAPQdBU$ATYQgQQHsMRQP9kLIIG12MNX62Gb04V.8Pl2.1hMPAN78CpR0qzLYvEuy3sjLw1/eJ90mAKqeSk9eJV.N/e9P0";
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
  };
}
