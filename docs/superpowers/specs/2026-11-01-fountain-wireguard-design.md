# Fountain-of-Ahmed-III WireGuard road-warrior client — design

## Goal

When the work Mac (Fountain-of-Ahmed-III, nix-darwin) is away from home,
Vaultwarden and Seafile on darktower keep working, alongside the work's
Jamf Trust "Private Access" VPN and without disturbing it. At home the
tunnel is off, because those services already work directly over the
intranet. A second, manually started profile can send all traffic home
when wanted.

## Context (verified 2026-11-01)

- Server side is stargate's existing road-warrior tunnel (see
  `2026-11-01-arrakis-wireguard-design.md`): `tun_wg0`, ROAD21
  `192.168.21.1/24`, endpoint `robot-disco.net:51820`, MTU 1412. ROAD21
  rules allow DNS to `192.168.21.1`, TCP/UDP 443 to darktower
  `192.168.50.99`, and the internet; RFC1918 is otherwise blocked.
- `robot-disco.net` has an A record and no AAAA.
- Jamf Trust "Private Access" (`com.jamf.trust`) uses Apple's Network
  Extension framework. When connected it adds `utun4` with an
  interface-scoped (`I`) default route, `10.0.76/22`, `10.142/16` and
  about 35 public `/32`s. It has no `192.168.x` routes. It also adds a
  supplemental resolver `fddd:dddd::` with no domain, ordered ahead of the
  Wi-Fi resolver.
- The App Store WireGuard app ("Home VPN", also Network Extension) never
  worked while Jamf was connected.
- At home the Mac is on `192.168.20.0/24`, gateway `192.168.20.1` (stargate,
  MAC `64:62:66:2f:7a:71`), DNS `192.168.20.1`, search domain
  `home.robot-disco.net`. Vaultwarden and Seafile work there directly.
- macOS 27.2 redacts the Wi-Fi SSID from `ipconfig getsummary`, for root
  too, even with `ipconfig setverbose 1`.
- nix-secrets already has a system recipient,
  `systems.Fountain-of-Ahmed-III` (root's SSH host key). No secret uses it
  yet.
- nix-darwin's `networking.wg-quick` (`modules/services/wg-quick.nix`):
  `peers.*.publicKey` is a plain string written into
  `/etc/wireguard/<name>.conf`; `privateKeyFile` and `presharedKeyFile` are
  bare-key files applied with `wg set` in `PostUp`. `autostart = true`
  creates a launchd daemon with `KeepAlive` (`NetworkState`,
  `SuccessfulExit`) running `wg-quick up`.
- nix-darwin has no `/etc/hosts` option; it only restores
  `/etc/hosts.before-nix-darwin` (`modules/networking/default.nix`).
- darktower's names in nix-config: `vaultwarden.robot-disco.net` and
  `fallcube.robot-disco.net` (Seafile, confirmed by the owner).
- stargate's public IP changes from time to time (EasyDNS DDNS).

## Decisions

"Owner" decided it; "Claude" is Claude's call, open to review.

| Topic | Decision | Why | By |
|---|---|---|---|
| Client stack | nix-darwin `networking.wg-quick` | A plain `utun` plus routes, no Network Extension, so it doesn't compete with Jamf; declarative | Owner, on Claude's recommendation |
| Profiles | `road21` (split, automatic) and `road21-full` (everything, manual) | Daily need is two services; full tunnel only on demand | Owner |
| Keys and peer | Both profiles share one keypair, one PSK and one stargate peer at `192.168.21.4/32`. They are never up at the same time | pfSense only cares which key talks; split vs full lives in the Mac's `AllowedIPs` | Claude |
| `road21` routing | `AllowedIPs = 192.168.50.99/32` | Only darktower is needed; a most-specific route coexists with Jamf's | Owner (split tunnel), Claude (prefix) |
| Name resolution | Fixed `/etc/hosts` entries mapping `vaultwarden.robot-disco.net` and `fallcube.robot-disco.net` to `192.168.50.99`; `road21` sets no DNS | `192.168.50.99` works everywhere (intranet at home, the `/32` away), so a static mapping can't point at the wrong place. No dependence on Jamf's DNS ordering, no resolver juggling. Cost: a new service or a new darktower IP means a config edit | Owner, chosen over a per-domain resolver |
| `road21-full` routing | `AllowedIPs = 0.0.0.0/0, ::/0`; `dns = 192.168.21.1` | All traffic home. `::/0` stops IPv6 leaking; stargate has no IPv6, so apps fall back to IPv4 | Claude |
| IPv6 address | `fd00:21::4/128` on both profiles | Mirrors arrakis (`fd00:21::2`); gives `::/0` a source address so IPv6 is sent into the tunnel, not around it | Claude |
| MTU / keepalive | 1412 / 25 s | Same as arrakis and stargate | Claude |
| Server public key | Plain text in nix-config | The module only takes a string; the alternative (peer and routes by hand in `postUp`) isn't worth it. Differs from arrakis, which keeps it in the secret | Owner |
| Secrets | Two bare-key agenix files in nix-secrets (private key, PSK), recipients `withRecoveryPersonal [ systems.Fountain-of-Ahmed-III ]` | The module wants bare-key files; it's a home-network secret, edited with the personal YubiKey | Claude |
| Key handling | Keys generated on the Mac and put straight into `agenix -e`; never printed in a shared session | Same rule as arrakis | Owner (standing rule) |
| At home | No tunnel: the toggle brings `road21` and `road21-full` down | Services already work over the intranet; the endpoint is unreachable from inside (no NAT reflection) | Owner |
| Home detection | Home = the default route's gateway has MAC `64:62:66:2f:7a:71` (stargate) | SSID is redacted on macOS 27; MAC needs no permissions and is harder to spoof | Owner |
| Toggle | A root launchd job reacting to network changes: home → both down; away and `road21-full` not up → `road21` up; otherwise nothing. Only acts on a change | The Mac version of arrakis's dispatcher script | Owner (behaviour), Claude (rules) |
| Endpoint re-resolution | The toggle job also checks whichever profile is up: if its latest handshake is older than ~135 s, it re-resolves `robot-disco.net` and runs `wg set <iface> peer <stargate key> endpoint <ip>:51820`. The lookup goes to a resolver outside the tunnel (e.g. `dig +short @1.1.1.1`), because under `road21-full` all DNS goes to `192.168.21.1`, which is unreachable while the endpoint is stale | DDNS changes stargate's IP; `wg-quick` resolves the endpoint only once. Re-pointing the peer in place keeps routes and keys; checking the handshake first avoids lookups while the tunnel is healthy | Owner (no manual restart), Claude (mechanism) |
| Autostart | `autostart = false` on both; the toggle owns `road21`, `road21-full` is started by hand (`sudo wg-quick up road21-full`) | The module's `KeepAlive` daemon would fight the toggle | Claude |
| Old setup | Remove the `"WireGuard"` `homebrew.masApps` entry; uninstall the app and delete the "Home VPN" configuration by hand; delete the old "Home VPN" peer on stargate | Replaced; `onActivation.cleanup` is unset, so removing the entry doesn't uninstall | Owner |

## Goals

- Away, with or without Jamf connected: `vaultwarden.robot-disco.net` and
  Seafile resolve to darktower and work.
- At home: the tunnel is down and nothing about DNS or routing changes.
- Jamf Private Access keeps working exactly as before.
- `road21-full` sends all traffic home when started by hand.

## Non-goals

- Enforcing that `road21-full` and Jamf are never connected together.
  That's on the owner; `road21-full` is manual.
- Captive-portal handling (same as arrakis).
- A module shared with arrakis: different stacks, secrets and routing.
- Changing stargate's firewall rules beyond the peer swap.

## Anti-goals

- Anything that breaks or degrades Jamf Private Access or work access.
- A tunnel that's up at home and blackholes darktower.
- The private key or PSK in the Nix store, git, logs or a shared session.
- `road21-full` coming up on its own.

## Constraints

- Work-managed Mac (Jamf). Changes stay inside nix-darwin and the two
  WireGuard profiles; Jamf's configuration isn't touched.
- First system-level agenix secret on this Mac: check `age.identityPaths`
  resolves to root's host key (arrakis needed it set explicitly because
  sshd was off).
- Secret edits only reach the Mac after commit, push and
  `nix flake update robotdisco-secrets`.
- Commits in nix-config run inside `nix develop` (the pre-commit hook needs
  `flake-checker`) and are GPG-signed with the YubiKey.

## Left to the builder

- How the launchd job notices network changes (`WatchPaths` on
  SystemConfiguration, polling, or similar) and how it reads the
  gateway's MAC. It must use the real default route (Jamf's scoped default
  and `road21-full`'s `0/1` + `128/1` must not fool it), react within
  about 30 s, never loop, and log what it does.
- Secret file names.
- How `/etc/hosts` gets its entries. It must be declared in nix-darwin,
  idempotent, leave the rest of the file alone (IT or Jamf may add
  entries), and be removable cleanly. Claude's suggestion: a marked block
  maintained by an activation script, rather than `environment.etc`
  replacing the whole file on a managed Mac.
- The exact re-resolve interval and handshake threshold, and which
  outside resolver to ask.
- A desk test for the toggle's decisions (fake commands, like arrakis's
  harness) before trying it live.
- Live checks away (the phone hotspot works): Jamf connected and not;
  `dscacheutil -q host -a name vaultwarden.robot-disco.net` returns
  `192.168.50.99`; Vaultwarden and Seafile load; `road21-full` gives the
  home WAN IP and fails `curl -6`; arriving home brings everything down
  and both names still work over the intranet. Re-resolution can be
  tested without a real IP change by pointing the peer at a wrong
  endpoint (`wg set … endpoint 192.0.2.1:51820`) and watching the job
  repair it.
