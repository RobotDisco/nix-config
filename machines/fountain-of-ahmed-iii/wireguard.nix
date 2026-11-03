{
  config,
  lib,
  pkgs,
  robotdisco-secrets,
  ...
}:

# Road-warrior WireGuard client for stargate (pfSense, tun_wg0), alongside
# Jamf Trust's Private Access. road21 reaches darktower only; road21-full
# sends everything home and is started by hand. See
# docs/superpowers/specs/2026-11-01-fountain-wireguard-design.md
let
  # stargate's tun_wg0 public key. Public; plain text by choice here.
  stargatePublicKey = "rlmz8pRM1lNrkqGXrygiSS9suYH4vEc4igk9Qc9lkh0=";

  # darktower's address works everywhere (intranet at home, road21 away),
  # so its names live in /etc/hosts rather than DNS
  darktower = "192.168.50.99";
  darktowerNames = [
    "vaultwarden.robot-disco.net"
    "fallcube.robot-disco.net"
  ];

  # Scripts live in their own files so the desk tests in ./tests can run
  # them with fake commands
  road21Hosts = pkgs.writeShellApplication {
    name = "road21-hosts";
    runtimeInputs = [ pkgs.gawk ];
    text = builtins.readFile ./road21-hosts.sh;
  };

  road21Toggle = pkgs.writeShellApplication {
    name = "road21-toggle";
    # route, arp, ifconfig, ipconfig and dig come from macOS via launchd's
    # PATH
    runtimeInputs = with pkgs; [
      wireguard-tools
      wireguard-go
      gawk
    ];
    text = builtins.readFile ./road21-toggle.sh;
  };

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
    road21 = profile [ "${darktower}/32" ] [ ];
    # Everything home, DNS included so lookups can't leak to the local net
    road21-full =
      profile
        [
          "0.0.0.0/0"
          "::/0"
        ]
        [ "192.168.21.1" ];
  };

  # Keeps the darktower block in /etc/hosts; the rest of the file (IT or
  # Jamf entries) is left alone. To remove the block before deleting this
  # module, set darktowerNames = [ ] and switch once: with no names the
  # script removes it.
  system.activationScripts.postActivation.text = lib.mkAfter ''
    ${lib.getExe road21Hosts} /etc/hosts ${darktower} ${lib.concatStringsSep " " darktowerNames}
    dscacheutil -flushcache
  '';

  # Location toggle and DDNS follow-up (see road21-toggle.sh)
  # Rotate the toggle's log with macOS's newsyslog: at 1 MB, keep 5,
  # bzip2-compressed (J)
  environment.etc."newsyslog.d/road21-toggle.conf".text = ''
    # logfilename                 mode count size(KB) when flags
    /var/log/road21-toggle.log    644  5     1024     *    J
  '';

  launchd.daemons.road21-toggle.serviceConfig = {
    ProgramArguments = [ (lib.getExe road21Toggle) ];
    StartInterval = 30;
    RunAtLoad = true;
    # wireguard-go and wg-quick's route monitor stay in this job's process
    # group, which launchd otherwise kills as soon as the toggle exits
    AbandonProcessGroup = true;
    StandardOutPath = "/var/log/road21-toggle.log";
    StandardErrorPath = "/var/log/road21-toggle.log";
  };
}
