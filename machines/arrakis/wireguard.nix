{
  config,
  lib,
  pkgs,
  robotdisco-secrets,
  ...
}:

# Road-warrior WireGuard client for stargate (pfSense, tun_wg0).
# Full tunnel, on only away from home: at home it would just send all
# traffic out and back through stargate. See
# docs/superpowers/specs/2026-11-01-arrakis-wireguard-design.md
let
  # NM dispatcher hook, called as `road21-toggle <interface> <action>`.
  # runtimeInputs pins PATH (the dispatcher's is minimal) and the build
  # runs shellcheck over the script.
  road21Toggle = pkgs.writeShellApplication {
    name = "road21-toggle";
    runtimeInputs = with pkgs; [
      networkmanager
      gnugrep
    ];
    text = builtins.readFile ./road21-toggle.sh;
  };

  # Brings road21 back down when it can't handshake (see road21-watchdog.sh)
  road21Watchdog = pkgs.writeShellApplication {
    name = "road21-watchdog";
    runtimeInputs = with pkgs; [
      networkmanager
      wireguard-tools
      gawk
      coreutils
    ];
    text = builtins.readFile ./road21-watchdog.sh;
  };
in
{
  # Env file with WG_LOCAL_PRIVATE_KEY, WG_REMOTE_PUBLIC_KEY and WG_PSK
  age.secrets.wireguard-arrakis.file = "${robotdisco-secrets}/wireguard-arrakis.age";

  networking = {
    networkmanager = {
      dispatcherScripts = [
        {
          source = lib.getExe road21Toggle;
          type = "basic";
        }
      ];

      ensureProfiles = {
        environmentFiles = [ config.age.secrets.wireguard-arrakis.path ];

        profiles.road21 = {
          connection = {
            id = "road21";
            type = "wireguard";
            interface-name = "road21";
            autoconnect = false;
          };
          wireguard = {
            private-key = "$WG_LOCAL_PRIVATE_KEY";
            # 1500 - 8 (PPPoE) - 80 (WireGuard over IPv6, worst case)
            mtu = 1412;
          };
          "wireguard-peer.$WG_REMOTE_PUBLIC_KEY" = {
            endpoint = "robot-disco.net:51820";
            allowed-ips = "0.0.0.0/0;::/0;";
            # Keep CGNAT/hotel NAT mappings alive
            persistent-keepalive = 25;
            preshared-key = "$WG_PSK";
            preshared-key-flags = 0;
          };
          ipv4 = {
            method = "manual";
            address1 = "192.168.21.2/32";
            dns = "192.168.21.1;";
            # Negative: use only the tunnel's DNS while it's up (no leaks)
            dns-priority = -50;
          };
          ipv6 = {
            # ULA so NM installs the ::/0 route and IPv6 can't leak around the
            # tunnel. stargate has no IPv6, so apps fall back to IPv4.
            method = "manual";
            address1 = "fd00:21::2/128";
          };
        };
      };
    };

    # stargate's encrypted replies arrive on Wi-Fi without NM's fwmark, so
    # the strict reverse-path lookup routes them to road21 and drops them.
    # Exempt just WireGuard's port rather than going "loose" everywhere.
    firewall = {
      extraCommands = ''
        ip46tables -t mangle -I nixos-fw-rpfilter -p udp --sport 51820 -j RETURN
      '';
      extraStopCommands = ''
        ip46tables -t mangle -D nixos-fw-rpfilter -p udp --sport 51820 -j RETURN || true
      '';
    };
  };

  systemd = {
    # road21's profile is written after NetworkManager starts, possibly
    # after Wi-Fi is already up, when the dispatcher's `up road21` found no
    # such connection. Decide once more now that it exists; a synthetic
    # "up" makes the script check where we are.
    services.NetworkManager-ensure-profiles.serviceConfig.ExecStartPost = [
      # "-": a failure here must not mark the profiles as failed
      "-${lib.getExe road21Toggle} wlp1s0 up"
    ];

    services.road21-watchdog = {
      description = "Bring road21 down when it can't reach stargate";
      serviceConfig = {
        Type = "oneshot";
        ExecStart = lib.getExe road21Watchdog;
      };
    };
    timers.road21-watchdog = {
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnBootSec = "1min";
        OnUnitActiveSec = "30s";
        # systemd may otherwise delay each run by up to a minute
        AccuracySec = "1s";
      };
    };
  };
}
