# arrakis WireGuard road-warrior client — design

## Goal

When arrakis is away from home, all of its traffic goes through the
WireGuard tunnel on stargate (pfSense). At home, the tunnel is off.

## Server side (already done, verified 2026-11-01 with the phone)

- Tunnel `tun_wg0`, assigned interface ROAD21, `192.168.21.1/24`, MTU 1412
- Endpoint `robot-disco.net:51820` (EasyDNS DDNS, dynamic WAN IP)
- ROAD21 rules: DNS to 192.168.21.1, TCP/UDP 443 to 192.168.50.99,
  block self, block RFC1918, pass internet
- No NAT reflection: at home, the endpoint resolves to the WAN IP and
  the handshake cannot complete

## Decisions

| Topic | Decision | Why |
|---|---|---|
| Client stack | NetworkManager profile via `networking.networkmanager.ensureProfiles` | arrakis already uses NM; `nmcli` and dispatcher scripts can toggle it |
| Location | `machines/arrakis/wireguard.nix` | Host-specific (own IP, own secret); the only Linux road warrior, so no reusable module (YAGNI) |
| Tunnel IP | `192.168.21.2/32` | Personal laptop slot (phone is .3, work laptop .4) |
| Routing | Full tunnel: `0.0.0.0/0` and `::/0` | Matches the phone; untrusted networks |
| IPv6 | ULA `fd00:21::2/128` on the tunnel | Gives NM a reason to install the `::/0` route so IPv6 can't leak; stargate has no IPv6, so apps fall back to IPv4 |
| DNS | `192.168.21.1`, `dns-priority = -50` | Negative priority makes NM use only the tunnel's DNS while it's up (no leaks); no systemd-resolved on arrakis |
| MTU | 1412 | 1500 − 8 (PPPoE) − 80 (WireGuard, IPv6 worst case) |
| Keepalive | 25 s | Keeps CGNAT/hotel NAT mappings alive |
| Secrets | `wireguard-arrakis.age` in nix-secrets, env-file format with `WG_LOCAL_PRIVATE_KEY`, `WG_PSK` and `WG_REMOTE_PUBLIC_KEY`, recipients `withRecoveryPersonal [ systems.arrakis ]` | Same pattern as `seafile-envs`; NM substitutes `$VARS` from `ensureProfiles.environmentFiles` |
| Key handling | Private key is generated on arrakis and goes straight into `agenix -e`; it is never printed in a shared session | The tunnel's private key leaked once already today |
| Server public key | Stored in the secret as `WG_REMOTE_PUBLIC_KEY`; the peer section is named `wireguard-peer.$WG_REMOTE_PUBLIC_KEY`, which `envsubst` fills in at runtime (it substitutes section headers too). Decided 2026-11-01, replacing plain text in nix-config | Keeps the repo from revealing which key goes with the home endpoint. Cost: an undefined variable becomes an empty string with no error, so a missing or misspelled key only shows up at runtime (`nmcli connection show road21`, handshake), not at eval or build |
| Firewall | Keep strict reverse-path filtering (`checkReversePath` default `true`); exempt UDP source port 51820 from the `nixos-fw-rpfilter` chain via `extraCommands`/`extraStopCommands` (iptables backend). No fwmark pin. Decided 2026-11-01, replacing `"loose"` | NM marks only WireGuard's own outgoing packets. stargate's encrypted replies arrive on Wi-Fi unmarked, the reverse lookup routes them to `road21`, so strict drops them. The port exemption is narrower than `"loose"`, which relaxes every interface, and WireGuard discards unauthenticated packets anyway. No rule matches the mark value, so pinning it buys nothing |
| Autoconnect | `autoconnect = false`; a NM dispatcher script brings it up away from home and down at home | Full tunnel at home cuts off the internet (no NAT reflection) |
| Home detection | Owner's choice: SSID, or SSID + gateway MAC | SSID is spoofable; the gateway MAC is harder to fake |

## Non-goals

- Work laptop (Fountain-of-Ahmed-III, macOS): separate design,
  `2026-11-01-fountain-wireguard-design.md`.
- Split-tunnel profile: add later if home upload proves too slow.
- Captive-portal detection: portals rarely come up, and it needs NM's
  connectivity check (off on arrakis; enabling it sends periodic requests
  to a check URL). Until then the tunnel comes up behind a portal; bring
  it down by hand to log in. TODO in `wireguard.nix`.

## Follow-ups

- DDNS endpoint changes: the plan's Task 2 Step 8 restarts `road21` by
  hand. Find out whether NetworkManager re-resolves a WireGuard peer's
  endpoint hostname on its own; if not, add a stale-handshake re-resolve
  (as in `2026-11-01-fountain-wireguard-design.md`) to the dispatcher or a
  timer.
