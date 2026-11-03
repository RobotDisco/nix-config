# arrakis WireGuard Road-Warrior Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps
> use checkbox (`- [ ]`) syntax for tracking.

**Goal:** arrakis sends all traffic through stargate's WireGuard tunnel
when away from home, and turns the tunnel off at home.

**Architecture:** A declarative NetworkManager WireGuard profile
(`ensureProfiles`) whose secrets come from an agenix env file. A NM
dispatcher script toggles the profile based on whether arrakis is on the
home network.

**Tech Stack:** NixOS 26.05, NetworkManager, ragenix, pfSense WireGuard
package.

**Spec:** `docs/superpowers/specs/2026-11-01-arrakis-wireguard-design.md`

## Global Constraints

- Tunnel IP `192.168.21.2/32`; ULA `fd00:21::2/128`; DNS `192.168.21.1`
- Endpoint `robot-disco.net:51820`; MTU `1412`; keepalive `25`
- AllowedIPs `0.0.0.0/0;::/0;`
- NM connection id and interface name: `road21`
- Secret file `wireguard-arrakis.age`, variables `WG_LOCAL_PRIVATE_KEY`, `WG_PSK`
- **No WireGuard key (private, public or preshared) is printed in a
  shared/agent session.** Key steps marked *(owner)* are done by the owner.
- Working tree has unrelated uncommitted changes in
  `home-manager/profiles/gaelan-personal-desktop.nix`,
  `modules/nixos/common/user-gaelan.nix`, `users/gaelan/default.nix`:
  stage files by path only, never `git add -A`.
- Repo hygiene: `just fmt`, no trailing whitespace, lines ≤ 80 chars.
- `just apply` is permanent: confirm with the owner before running.

## Review Focus

1. **IPv6 leak on a dual-stack network** → `curl -6` must fail while the
   tunnel is up (Task 2, Step 7).
2. **DNS leak**: `/etc/resolv.conf` must list only `192.168.21.1` while
   the tunnel is up (Task 2, Step 7).
3. **Dispatcher loop**: the script reacting to `road21`'s own up/down
   events must not re-toggle it (Task 3, Step 4).
4. **Leaving home with Wi-Fi off / no network**: the dispatcher must not
   spam `nmcli con up` when there's no connectivity (Task 3, Step 4).
5. **WAN IP change**: after a DDNS update, restarting `road21` must
   reconnect (Task 2, Step 8).

---

### Task 1: Keys, pfSense peer, and the encrypted secret

**Files:**
- Modify: `~/code/nix-secrets/secrets.nix`
- Create: `~/code/nix-secrets/wireguard-arrakis.age`
- Modify: `~/code/nix-config/flake.lock` (robotdisco-secrets input)

**Interfaces:**
- Produces: `wireguard-arrakis.age`, an env file with exactly two lines,
  `WG_LOCAL_PRIVATE_KEY=<key>` and `WG_PSK=<key>`, decryptable by
  `systems.arrakis`

- [ ] **Step 1 (owner): Generate the key pair on arrakis**

```sh
umask 077; wg genkey | tee /dev/shm/wg-arrakis.key | wg pubkey
```
Expected: one line, the **public** key. Paste it into pfSense in Step 2.

- [ ] **Step 2 (owner): Add the pfSense peer**

VPN → WireGuard → Peers → Add: tunnel `tun_wg0`, description `Laptop`,
dynamic endpoint, public key from Step 1, Allowed IPs `192.168.21.2/32`,
click *Generate* for a pre-shared key and copy it. Save and apply.

- [ ] **Step 3: Add the recipient entry**

In `secrets.nix`:
`"wireguard-arrakis.age" = withRecoveryPersonal [ systems.arrakis ];`

- [ ] **Step 4 (owner): Create the secret**

In `~/code/nix-secrets` (YubiKey present): `agenix -e wireguard-arrakis.age`
and enter `WG_LOCAL_PRIVATE_KEY=` (contents of `/dev/shm/wg-arrakis.key`) and
`WG_PSK=` (from Step 2). Then `shred -u /dev/shm/wg-arrakis.key`.

- [ ] **Step 5: Verify the secret's shape without revealing it**

**Not** `agenix -d … | cut -d= -f1`: ragenix has no `-d`, and `cut` prints
any line without `=` in full, which would show a key. In a real terminal
(the YubiKey needs a PIN), with an `EDITOR` that prints only names and
lengths, so the file is left unchanged:

```sh
cat > /dev/shm/shape.sh <<'SH'
#!/bin/sh
awk '{ if (index($0,"=")) print substr($0,1,index($0,"=")-1) " (value length " length($0)-index($0,"=") ")"; else print "<line without = : not shown>" } END { print NR " lines" }' "$1"
SH
chmod +x /dev/shm/shape.sh
EDITOR=/dev/shm/shape.sh nix-shell --run 'agenix -i identities/personal.identity -e wireguard-arrakis.age'
```
Expected: `WG_LOCAL_PRIVATE_KEY`, `WG_REMOTE_PUBLIC_KEY` and `WG_PSK`, each
`(value length 44)`, then `3 lines`.

- [ ] **Step 6: Commit and push nix-secrets, then update the lock**

```sh
git -C ~/code/nix-secrets add secrets.nix wireguard-arrakis.age
git -C ~/code/nix-secrets commit -m "feat: add wireguard-arrakis secret"
git -C ~/code/nix-secrets push
nix flake update robotdisco-secrets  # in ~/code/nix-config
```

---

### Task 2: NetworkManager WireGuard profile

**Files:**
- Create: `machines/arrakis/wireguard.nix`
- Modify: `machines/arrakis/default.nix` (add `./wireguard.nix` to
  `imports`)

**Interfaces:**
- Consumes: `wireguard-arrakis.age` (Task 1)
- Produces: NM connection `road21`, toggled with
  `nmcli connection up|down road21`

- [ ] **Step 1: Write the failing eval checks**

```sh
A=.#nixosConfigurations.arrakis.config
nix eval $A.networking.networkmanager.ensureProfiles.profiles.road21.wireguard.mtu
nix eval $A.networking.firewall.checkReversePath
nix eval --raw $A.networking.firewall.extraCommands | grep -c 'sport 51820'
```
Expected after implementation: `1412`, `true` and `1`.

- [ ] **Step 2: Run them to verify they fail**

Expected: first errors with `attribute 'road21' missing`; second prints
`true`.

- [ ] **Step 3: Implement `machines/arrakis/wireguard.nix`**

- `age.secrets.wireguard-arrakis.file =
  "${robotdisco-secrets}/wireguard-arrakis.age";`
- `networking.networkmanager.ensureProfiles.environmentFiles =
  [ config.age.secrets.wireguard-arrakis.path ];`
- `profiles.road21` sections, values from Global Constraints:
  `connection` (id, `type = "wireguard"`, interface-name,
  `autoconnect = false`), `wireguard` (`private-key = "$WG_LOCAL_PRIVATE_KEY"`,
  mtu), `"wireguard-peer.<stargate tunnel public key>"` (endpoint,
  allowed-ips, persistent-keepalive, `preshared-key = "$WG_PSK"`,
  `preshared-key-flags = 0`), `ipv4` (`method = "manual"`, address1,
  dns, `dns-priority = -50`), `ipv6` (`method = "manual"`, address1).
- Strict reverse-path filtering stays on; `networking.firewall`
  `extraCommands` inserts (`-I`) and `extraStopCommands` deletes (`-D`,
  `|| true`) `ip46tables -t mangle ... nixos-fw-rpfilter -p udp --sport
  51820 -j RETURN`, with a comment saying why (spec, Firewall row).
- *(owner)* pastes the stargate tunnel's public key into the peer
  section name.

- [ ] **Step 4: Run the eval checks to verify they pass**

Expected: `1412`, `true` and `1`.

- [ ] **Step 5: Format, lint, build**

Run: `just fmt && just lint && just build-arrakis`
Expected: all succeed.

- [ ] **Step 6: Activate non-permanently**

Run: `just test-switch`, then `nmcli connection show road21`.
Expected: profile exists; `connection.autoconnect: no`.

- [ ] **Step 7: Live test off-site (owner, on the phone's hotspot)**

```sh
nmcli connection up road21
sudo wg show road21 | grep -E 'handshake|transfer'
curl -4 https://ifconfig.me          # home WAN IP
curl -6 --max-time 5 https://ifconfig.me; echo "exit=$?"  # must fail
grep nameserver /etc/resolv.conf     # only 192.168.21.1
sudo iptables -t mangle -vL nixos-fw-rpfilter | grep 51820  # pkts > 0
curl -sI https://vaultwarden.robot-disco.net | head -1
```
Expected: handshake and transfer lines present; home WAN IP; `curl -6`
non-zero exit; single nameserver; HTTP status line. If `curl -6`
succeeds, IPv6 is leaking: stop and revisit the `ipv6` section.

- [ ] **Step 8: Reconnect after the endpoint changes**

Run: `nmcli connection down road21 && nmcli connection up road21`, then
the handshake check from Step 7. Expected: a fresh handshake.

- [ ] **Step 9: Commit**

```sh
git add machines/arrakis/wireguard.nix machines/arrakis/default.nix \
  flake.lock docs/superpowers
git commit -m "feat(arrakis): add WireGuard road-warrior profile"
```

---

### Task 3: Auto-toggle with a NetworkManager dispatcher script

**Files:**
- Modify: `machines/arrakis/wireguard.nix`

**Interfaces:**
- Consumes: NM connection `road21` (Task 2)
- Produces: `networking.networkmanager.dispatcherScripts` entry,
  `type = "basic"`, invoked by NM as `<script> <interface> <action>`

- [ ] **Step 1: Write the failing eval check**

```sh
nix eval .#nixosConfigurations.arrakis.config.networking.networkmanager.dispatcherScripts --apply builtins.length
```
Expected after implementation: `1`. Now: `0`.

- [ ] **Step 2 (owner): Decide what "home" means**

SSID only, or SSID + gateway MAC (spec, Home detection row). Record the
choice in a comment at the top of the script.

- [ ] **Step 3 (owner): Write the decision logic**

`pkgs.writeShellScript "road21-toggle"` taking `$1` (interface) and
`$2` (action). Rules: ignore events where `$1` is `road21`; act only on
`up`, `down`, `connectivity-change`; home → `nmcli connection down
road21`; away with a working uplink → `nmcli connection up road21`; no
uplink → do nothing. Use store paths for `nmcli` (repo package-reference
convention: dispatcher has a minimal `$PATH`).

- [ ] **Step 4: Verify the behaviour matrix (owner)**

`just test-switch`, then, watching `journalctl -fu NetworkManager-dispatcher`:

| Situation | Expected `nmcli -t -f NAME connection show --active` |
|---|---|
| Join home Wi-Fi | no `road21` |
| Switch to phone hotspot | `road21` appears within ~10 s |
| `nmcli connection down road21` while on hotspot | stays down until the next network change (no loop) |
| Wi-Fi off | no repeated `con up` attempts in the journal |
| Back to home Wi-Fi | `road21` gone, internet works |

- [ ] **Step 5: Format, lint, commit**

```sh
just fmt && just lint
git add machines/arrakis/wireguard.nix
git commit -m "feat(arrakis): toggle WireGuard by network location"
```

---

### Task 4: Make it permanent

- [ ] **Step 1: Confirm with the owner, then `just apply`**
- [ ] **Step 2: Reboot, then repeat Task 3 Step 4's first two rows**

---

### Task 5: Watchdog for a road21 that can't handshake

Spec: Watchdog row. Added 2026-11-01 after the live hotspot tests.

**Files:**
- Create: `machines/arrakis/road21-watchdog.sh`
- Create: `machines/arrakis/tests/road21-watchdog-test.sh`
- Modify: `machines/arrakis/wireguard.nix`

**Interfaces:**
- Consumes: NM connection `road21`; `wg show road21 latest-handshakes`
  (`<peer key>\t<epoch>`, `0` = never)
- Produces: `systemd.services.road21-watchdog` (oneshot, root) and
  `systemd.timers.road21-watchdog` (`OnBootSec = "1min"`,
  `OnUnitActiveSec = "30s"`). State in `$STATE_DIR` (default
  `/run/road21-watchdog`): `up-since` (epoch), `retried` (flag)

Behaviour (one run): `road21` not active → clear state, exit. No
`up-since` → write now, exit. Healthy (handshake within 180 s) → clear
`retried`, exit. No handshake and up ≤ 60 s → exit. Otherwise unhealthy:
no `retried` → set it, reset `up-since`, `nmcli --wait 0 connection up
road21` (reconnect); `retried` set → `nmcli --wait 0 connection down
road21`, clear state. Log one line per action.

- [ ] **Step 1: Write the failing desk test** (fakes for `nmcli`, `wg`,
  `date`; `STATE_DIR` in a temp dir; `bash -euo pipefail`):

| Case | Setup | Expected action |
|---|---|---|
| not active | road21 not in active list, stale state present | none; state cleared |
| first sight | active, no state | none; `up-since` written |
| starting | active, up 30 s, handshake 0 | none |
| healthy | handshake 10 s ago, `retried` set | none; `retried` cleared |
| never handshook | up 90 s, handshake 0 | `nmcli --wait 0 connection up road21`; `retried` set |
| still nothing after retry | up 90 s, handshake 0, `retried` | `nmcli --wait 0 connection down road21`; state cleared |
| went stale | handshake 300 s ago | reconnect (`up`) |
| stale after retry | handshake 300 s ago, `retried` | `down` |

- [ ] **Step 2: Run it** — `nix shell nixpkgs#gawk -c bash machines/arrakis/tests/road21-watchdog-test.sh` → FAIL (script missing)
- [ ] **Step 3: Implement `road21-watchdog.sh`**; constants `START_GRACE=60`, `STALE_AFTER=180`
- [ ] **Step 4: Run test + shellcheck** → 8/8 PASS, silent
- [ ] **Step 5: Wire service + timer** (`writeShellApplication`, `runtimeInputs` networkmanager, wireguard-tools, gawk, coreutils). Check:
  `nix eval .#nixosConfigurations.arrakis.config.systemd.timers.road21-watchdog.timerConfig.OnUnitActiveSec` → `"30s"`
- [ ] **Step 6 (owner): `just test-switch`, then live** — on the hotspot *with the phone on home Wi-Fi* (endpoint unreachable): within ~3 min the journal (`journalctl -u road21-watchdog`) shows one reconnect, then `down`, and internet works. With the phone on cellular: no watchdog actions.
- [ ] **Step 7: Commit**

---

## Addendum (2026-11-02): where the code ended up

The tasks above are the plan as executed, kept for the record. Live
testing changed several of them; the spec's Decisions table, the code and
`machines/arrakis/tests/` are authoritative.

**Secret and keys (Task 1)**
- The secret holds three variables: `WG_LOCAL_PRIVATE_KEY`,
  `WG_REMOTE_PUBLIC_KEY` (stargate's key, in the peer section name) and
  `WG_PSK`. Check its shape with the names-and-lengths `EDITOR` script
  (Step 5), never `cut`.
- `age.identityPaths` is set explicitly in `machines/arrakis/default.nix`:
  openssh is off, so agenix can't find the host key on its own.

**Profile and firewall (Task 2)**
- The connection and interface are called `road21` (pfSense's ROAD21),
  not `wg-home`.
- Strict reverse-path filtering stays on, with UDP source port 51820
  exempted, instead of `checkReversePath = "loose"`.
- `interfaces.wlp1s0.useDHCP` is gone: it ran dhcpcd alongside
  NetworkManager and leaked the local DNS server into `resolv.conf`.

**Dispatcher (Task 3)**
- Built with `writeShellApplication` (pinned `PATH`, shellcheck at build
  time). `location()` (owner's) counts only fully `activated`
  connections and treats an `nmcli` failure as offline. Home = an active
  connection named "Anor Londovercourt" (SSID only).
- Every `nmcli` change uses `--wait 0`: a blocking `nmcli connection up`
  from the dispatcher deadlocked with NetworkManager's pre-up wait.
- Desk test: `machines/arrakis/tests/road21-toggle-test.sh`, run against
  the built script (9/9).

**Added after the plan**
- Task 5, the watchdog, with `AccuracySec = "1s"` (watchdog test 8/8).
- Boot re-check: `NetworkManager-ensure-profiles` runs the toggle once
  after writing the profiles, in case Wi-Fi came up first (spec, Boot
  re-check row).

**Corrected premise:** the handshake does work from inside the home
network. The tunnel stays off at home because it adds nothing there, not
because it would fail.

**Live results (2026-11-01):** on a cellular hotspot, the tunnel, DNS and
IPv6 behave as specified. Home↔away switching is clean; the setup survives
a reboot; with UDP 51820 blocked, the watchdog reconnects once and then
restores internet within about 2 minutes.

**Still open:** an IPv6 check on a dual-stack network, Task 2 Step 8
(reconnect after an endpoint change), the firewall counter, and a reboot
while away from home (to exercise the boot re-check).
