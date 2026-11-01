{ config, robotdisco-secrets, ... }:

# Road-warrior WireGuard client for stargate (pfSense, tun_wg0), alongside
# Jamf Trust's Private Access. road21 reaches darktower only; road21-full
# sends everything home and is started by hand. See
# docs/superpowers/specs/2026-11-01-fountain-wireguard-design.md
let
  # stargate's tun_wg0 public key. Public; plain text by choice here.
  stargatePublicKey = "REPLACE-WITH-STARGATE-TUNNEL-PUBLIC-KEY";

  # Both profiles share one key and one stargate peer (192.168.21.4) and
  # are never up together; only the routes differ.
  profile = allowedIPs: dns: {
    address = [
      "192.168.21.4/32"
      # ULA so ::/0 has a source address and IPv6 can't leak around
      # road21-full. stargate has no IPv6, so apps fall back to IPv4.
      "fd00:21::4/128"
    ];
    inherit dns;
    # 1500 - 8 (PPPoE) - 80 (WireGuard over IPv6, worst case)
    mtu = 1412;
    privateKeyFile = config.age.secrets.wireguard-fountain-private-key.path;
    # The location toggle owns road21; road21-full is manual
    autostart = false;
    peers = [
      {
        publicKey = stargatePublicKey;
        presharedKeyFile = config.age.secrets.wireguard-fountain-psk.path;
        endpoint = "robot-disco.net:51820";
        inherit allowedIPs;
        # Keep CGNAT/hotel NAT mappings alive
        persistentKeepalive = 25;
      }
    ];
  };
in
{
  age.secrets = {
    wireguard-fountain-private-key.file = "${robotdisco-secrets}/wireguard-fountain-private-key.age";
    wireguard-fountain-psk.file = "${robotdisco-secrets}/wireguard-fountain-psk.age";
  };

  networking.wg-quick.interfaces = {
    # darktower only; names come from /etc/hosts, so no DNS here
    road21 = profile [ "192.168.50.99/32" ] [ ];
    # Everything home, DNS included so lookups can't leak to the local net
    road21-full =
      profile
        [
          "0.0.0.0/0"
          "::/0"
        ]
        [ "192.168.21.1" ];
  };
}
