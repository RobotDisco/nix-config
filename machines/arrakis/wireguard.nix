{ config, robotdisco-secrets, ... }:

# Road-warrior WireGuard client for stargate (pfSense, tun_wg0).
# Full tunnel; toggled by location since the endpoint is unreachable from
# home (no NAT reflection). See
# docs/superpowers/specs/2026-11-01-arrakis-wireguard-design.md
{
  # Env file with WG_LOCAL_PRIVATE_KEY, WG_REMOTE_PUBLIC_KEY and WG_PSK
  age.secrets.wireguard-arrakis.file = "${robotdisco-secrets}/wireguard-arrakis.age";

  networking.networkmanager.ensureProfiles = {
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

  # stargate's encrypted replies arrive on Wi-Fi without NM's fwmark, so
  # the strict reverse-path lookup routes them to road21 and drops them.
  # Exempt just WireGuard's port rather than going "loose" everywhere.
  networking.firewall = {
    extraCommands = ''
      ip46tables -t mangle -I nixos-fw-rpfilter -p udp --sport 51820 -j RETURN
    '';
    extraStopCommands = ''
      ip46tables -t mangle -D nixos-fw-rpfilter -p udp --sport 51820 -j RETURN || true
    '';
  };
}
