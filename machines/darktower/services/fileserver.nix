{ pkgs, ... }:

{
  networking.firewall.interfaces.br20.allowedTCPPorts = [
    139
    445
  ];

  containers = {
    fileserver = {
      autoStart = true;
      privateNetwork = true;
      hostBridge = "br20";
      localAddress = "192.168.20.2/24";
      bindMounts = {
        "/srv/archive" = {
          hostPath = "/srv/storagepool/archive";
          isReadOnly = false;
        };
      };

      config = {
        system.stateVersion = "22.05";

        programs.zsh.enable = true;

        # Sadly we need firewall access both inside the container and on the
        # host system.
        networking.firewall.allowedTCPPorts = [
          139
          445
        ];

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
          settings = {
            global = {
              security = "user";

              "hosts allow" = "192.168.20. 127.0.0.1";
              "hosts deny" = "0.0.0.0/0";

              "netbios name" = "chapterhouse";

              "guest account" = "nobody";
              "map to guest" = "bad user";

              "server string" = "chapterhouse";
              workgroup = "ROBOT-DISCO";
            };

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
