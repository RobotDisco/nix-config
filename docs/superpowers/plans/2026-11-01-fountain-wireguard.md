# Fountain-of-Ahmed-III WireGuard Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps
> use checkbox (`- [ ]`) syntax for tracking.

**Goal:** The work Mac reaches Vaultwarden and Seafile on darktower away
from home through a split WireGuard tunnel that coexists with Jamf, turns
it off at home, and offers a manual full tunnel.

**Architecture:** Two nix-darwin `networking.wg-quick` profiles sharing one
key. `/etc/hosts` maps darktower's names. A root launchd job runs every
30 s: it picks the profile from the default gateway's MAC and re-points
the peer when the handshake goes stale (DDNS). Both shell scripts live in
their own `.sh` files so desk tests can run them with fake commands.

**Tech Stack:** nix-darwin 26.05 (`networking.wg-quick`, `launchd.daemons`,
`system.activationScripts`), ragenix, wireguard-tools, bash, macOS 27.2.

**Spec:** `docs/superpowers/specs/2026-11-01-fountain-wireguard-design.md`

## Global Constraints

- Tunnel addresses `192.168.21.4/32` and `fd00:21::4/128`; endpoint
  `robot-disco.net:51820`; MTU `1412`; keepalive `25`
- `road21`: `AllowedIPs 192.168.50.99/32`, no DNS. `road21-full`:
  `AllowedIPs 0.0.0.0/0, ::/0`, DNS `192.168.21.1`. Both `autostart = false`
- Home = default gateway MAC `64:62:66:2f:7a:71`
- `/etc/hosts`: `192.168.50.99` for `vaultwarden.robot-disco.net` and
  `fallcube.robot-disco.net`
- Stale handshake = older than `135` s; toggle interval `30` s
- Secrets `wireguard-fountain-private-key.age` and
  `wireguard-fountain-psk.age`: one bare key per file, recipients
  `withRecoveryPersonal [ systems.Fountain-of-Ahmed-III ]`
- **No WireGuard private key or PSK printed in a shared/agent session.**
  Steps marked *(owner)* are done by the owner.
- Branch `arrakis-wireguard` (shared with the arrakis work). Stage by
  path; the working tree has unrelated uncommitted files.
- Commit inside `nix develop` (pre-commit needs `flake-checker`); commits
  are GPG-signed with the YubiKey, so the owner may have to run them.
- Nix evals and desk tests run anywhere (arrakis is fine). Building and
  switching happen on the Mac. On darwin, `just test-switch` is a real
  switch; confirm with the owner before each one.
- Scripts must behave the same on macOS and Linux: use `awk` from
  `pkgs.gawk` (in `runtimeInputs`), never `sed -i`.

## Review Focus

1. **Gateway's MAC not known yet** (`arp` shows `(incomplete)` right after
   joining a network) → do nothing; never treat it as away (Task 4 case
   `arp incomplete`).
2. **`road21-full` endpoint change**: after `wg set … endpoint`, the new
   IP has no host route via the real gateway and would loop into the
   tunnel → add `route -n add -host <ip> <gw>` (Task 4 case `full stale`;
   live in Task 5).
3. **Gateway lookup fooled** by Jamf's scoped default or
   `road21-full`'s `0/1` + `128/1` → the script reads `route -n get
   default`, which returns the unscoped primary (live in Task 5, Jamf and
   `road21-full` both up).
4. **No usable DHCP DNS server, or an empty answer** during re-resolve →
   leave the endpoint alone and exit 0 (Task 4 cases `no dhcp dns`,
   `dig empty`).
5. **Other `/etc/hosts` lines** (IT or Jamf entries) → kept byte for byte;
   re-running is a no-op (Task 3 tests).

---

### Task 1: Keys, stargate peer swap, and the encrypted secrets

**Files:**
- Modify: `~/code/nix-secrets/secrets.nix`
- Create: `~/code/nix-secrets/wireguard-fountain-private-key.age`,
  `~/code/nix-secrets/wireguard-fountain-psk.age`
- Modify: `flake.lock` (robotdisco-secrets input)

**Interfaces:**
- Produces: two age files, each exactly one line of 44 base64 characters,
  decryptable by `systems.Fountain-of-Ahmed-III`

- [ ] **Step 1 (owner, on the Mac): Generate the key pair**

```sh
umask 077; nix shell nixpkgs#wireguard-tools -c sh -c \
  'wg genkey | tee /tmp/wg-fountain.key | wg pubkey'
```
Expected: one line, the public key. Check `ls -l /tmp/wg-fountain.key`
shows `-rw-------`.

- [ ] **Step 2 (owner): Swap the stargate peer**

pfSense → VPN → WireGuard → Peers: delete the old "Home VPN" peer. Add:
tunnel `tun_wg0`, description `Fountain-of-Ahmed-III`, dynamic endpoint,
the public key from Step 1, Allowed IPs `192.168.21.4/32`, *Generate* a
pre-shared key and copy it. Save and apply. Note stargate's tunnel public
key (VPN → WireGuard → Tunnels → `tun_wg0`) for Task 2.

- [ ] **Step 3: Add the recipient entries to `secrets.nix`**

`"wireguard-fountain-private-key.age"` and `"wireguard-fountain-psk.age"`,
each `= withRecoveryPersonal [ systems.Fountain-of-Ahmed-III ];`.

- [ ] **Step 4 (owner, real terminal): Create the secrets**

`cd ~/code/nix-secrets && nix-shell --run 'agenix -e <file>'` for each.
Contents: the private key from `/tmp/wg-fountain.key` (e.g. `:r` in vim),
and the PSK from Step 2. One line, nothing else. Then
`rm -P /tmp/wg-fountain.key`.

- [ ] **Step 5 (owner, real terminal): Verify shape without revealing**

**Do not reuse arrakis's shape script**: it prints the text before the
first `=`, and a bare key ends in `=`, so it would print 43 characters of
the key. Use a length-only script instead:

```sh
cat > /tmp/len.sh <<'EOF'
#!/bin/sh
awk '{ print "line " NR ": length " length($0) } END { print NR " lines" }' "$1"
EOF
chmod +x /tmp/len.sh
```
For each file: `EDITOR=/tmp/len.sh nix-shell --run 'agenix -i identities/personal.identity -e <file>'`.
Expected: `line 1: length 44`, `1 lines`, and "wasn't changed, skipping
re-encryption".

- [ ] **Step 6: Commit, push, update the lock**

```sh
git -C ~/code/nix-secrets add secrets.nix wireguard-fountain-*.age
git -C ~/code/nix-secrets commit -m "feat: add wireguard keys for fountain"
git -C ~/code/nix-secrets push        # owner: YubiKey SSH
nix flake update robotdisco-secrets   # in nix-config, owner: YubiKey SSH
```
Check: the locked rev matches `git -C ~/code/nix-secrets rev-parse HEAD`.

---

### Task 2: Host directory and the two wg-quick profiles

**Files:**
- Move: `machines/fountain-of-ahmed-iii.nix` →
  `machines/fountain-of-ahmed-iii/default.nix` (fix its relative imports,
  e.g. `../modules` → `../../modules`)
- Modify: `flake.nix` (module path)
- Create: `machines/fountain-of-ahmed-iii/wireguard.nix` (imported from
  `default.nix`)

**Interfaces:**
- Consumes: the two secrets (Task 1)
- Produces: `networking.wg-quick.interfaces.road21` and `.road21-full`;
  `/var/run/wireguard/<name>.name` holds the `utunN` name while up

- [ ] **Step 1: Write the failing eval check**

```sh
nix eval --json .#darwinConfigurations.fountain-of-ahmed-iii.config \
  --apply 'c: let i = c.networking.wg-quick.interfaces;
    s = n: { inherit (i.${n}) address autostart dns mtu;
      pk = i.${n}.privateKeyFile == c.age.secrets.wireguard-fountain-private-key.path;
      peers = map (p: { inherit (p) allowedIPs endpoint persistentKeepalive;
        psk = p.presharedKeyFile == c.age.secrets.wireguard-fountain-psk.path; }) i.${n}.peers; };
    in { names = builtins.attrNames i; road21 = s "road21"; full = s "road21-full";
         ids = c.age.identityPaths != [ ]; }'
```
Expected after implementation:
`{"full":{"address":["192.168.21.4/32","fd00:21::4/128"],"autostart":false,"dns":["192.168.21.1"],"mtu":1412,"peers":[{"allowedIPs":["0.0.0.0/0","::/0"],"endpoint":"robot-disco.net:51820","persistentKeepalive":25,"psk":true}],"pk":true},"ids":true,"names":["road21","road21-full"],"road21":{"address":["192.168.21.4/32","fd00:21::4/128"],"autostart":false,"dns":[],"mtu":1412,"peers":[{"allowedIPs":["192.168.50.99/32"],"endpoint":"robot-disco.net:51820","persistentKeepalive":25,"psk":true}],"pk":true}}`

Also: `nix eval --raw .#darwinConfigurations.fountain-of-ahmed-iii.config.environment.etc."wireguard/road21.conf".text | grep -ci privatekey`
→ `0` (the key stays out of the store).

- [ ] **Step 2: Run it to verify it fails**

Expected: `error: … attribute 'road21' missing` (no interfaces yet).

- [ ] **Step 3: Move the host file and implement `wireguard.nix`**

`age.secrets.wireguard-fountain-private-key.file` and `…-psk.file` from
`robotdisco-secrets`; both interfaces from Global Constraints. A `let`
binding `stargatePublicKey` holds stargate's tunnel key (owner pastes it,
plain text by spec). Share one peer attrset between the profiles, varying
only `allowedIPs` (and `dns` on the interface).

- [ ] **Step 4: Run the checks to verify they pass**

Expected: the JSON above, and `0`.

- [ ] **Step 5 (owner, on the Mac): Build**

`git pull` the branch, then `just build-fountain-of-ahmed-iii` → succeeds.

- [ ] **Step 6 (owner, confirm first): Switch and test by hand, away**

`just test-switch` (real switch on darwin). On the phone hotspot, Jamf off:
```sh
sudo wg-quick up road21 && sudo wg show | grep -E 'handshake|transfer'
curl -sI --resolve vaultwarden.robot-disco.net:443:192.168.50.99 \
  https://vaultwarden.robot-disco.net | head -1
sudo wg-quick down road21
```
Expected: a handshake, an HTTP status line.

- [ ] **Step 7: Format, commit**

```sh
just fmt
git add flake.nix machines/fountain-of-ahmed-iii machines/fountain-of-ahmed-iii.nix flake.lock
nix develop -c git commit -m "feat(fountain): add road21 WireGuard profiles"
```

---

### Task 3: `/etc/hosts` block for darktower

**Files:**
- Create: `machines/fountain-of-ahmed-iii/road21-hosts.sh`
- Create: `machines/fountain-of-ahmed-iii/tests/road21-hosts-test.sh`
- Modify: `machines/fountain-of-ahmed-iii/wireguard.nix`

**Interfaces:**
- Produces: `road21-hosts FILE [IP NAME...]`. With an IP and names it
  writes exactly one block between `# BEGIN road21 (nix-darwin)` and
  `# END road21 (nix-darwin)`, one `IP NAME` line per name. With only
  FILE it removes the block. Everything outside the block is kept byte
  for byte. It rewrites in place (`cat tmp > FILE`) so ownership and mode
  stay.

- [ ] **Step 1: Write the failing desk test**

`tests/road21-hosts-test.sh` runs the script under `bash -euo pipefail`
against temp files and asserts, printing `PASS`/`FAIL` per case and
exiting non-zero on any failure:
- `adds block`: `127.0.0.1 localhost\n10.1.2.3 corp.example\n` → those two
  lines unchanged, then the block with `192.168.50.99 vaultwarden.robot-disco.net`
  and `192.168.50.99 fallcube.robot-disco.net`
- `idempotent`: running twice gives an identical file, one `# BEGIN`
- `replaces`: re-running with only `vaultwarden…` leaves one name
- `removes`: `road21-hosts FILE` restores the original bytes exactly
  (`cmp` against a copy)
- `no trailing newline`: the original last line isn't merged with
  `# BEGIN`

- [ ] **Step 2: Run it to verify it fails**

Run: `nix shell nixpkgs#gawk -c bash machines/fountain-of-ahmed-iii/tests/road21-hosts-test.sh`
Expected: FAIL (script missing).

- [ ] **Step 3: Implement `road21-hosts.sh`**

awk filters out any existing block, then appends the new one if there are
names.

- [ ] **Step 4: Run test and shellcheck**

Run: the Step 2 command, plus `nix run nixpkgs#shellcheck -- machines/fountain-of-ahmed-iii/road21-hosts.sh`
Expected: all PASS; shellcheck silent.

- [ ] **Step 5: Wire it into activation**

In `wireguard.nix`: `road21Hosts = pkgs.writeShellApplication { name =
"road21-hosts"; runtimeInputs = [ pkgs.gawk ]; text = builtins.readFile
./road21-hosts.sh; };` and `system.activationScripts.postActivation.text
= lib.mkAfter "${lib.getExe road21Hosts} /etc/hosts 192.168.50.99
vaultwarden.robot-disco.net fallcube.robot-disco.net\ndscacheutil
-flushcache\n";`

Check: `nix eval --raw .#darwinConfigurations.fountain-of-ahmed-iii.config.system.activationScripts.postActivation.text | grep -c 'road21-hosts /etc/hosts 192.168.50.99 vaultwarden.robot-disco.net fallcube.robot-disco.net'`
→ `1`.

- [ ] **Step 6 (owner, on the Mac, confirm first): Switch and check**

`just test-switch`, then `grep -A3 'BEGIN road21' /etc/hosts` and
`dscacheutil -q host -a name fallcube.robot-disco.net` →
`192.168.50.99`. At home, Seafile and Vaultwarden still load.

- [ ] **Step 7: Commit**

```sh
git add machines/fountain-of-ahmed-iii
nix develop -c git commit -m "feat(fountain): map darktower names in /etc/hosts"
```

---

### Task 4: Location toggle and endpoint re-resolve

**Files:**
- Create: `machines/fountain-of-ahmed-iii/road21-toggle.sh`
- Create: `machines/fountain-of-ahmed-iii/tests/road21-toggle-test.sh`
- Modify: `machines/fountain-of-ahmed-iii/wireguard.nix`

**Interfaces:**
- Consumes: profiles `road21`, `road21-full` (Task 2); a profile is up
  iff `$WG_RUN_DIR/<name>.name` exists (`WG_RUN_DIR` defaults to
  `/var/run/wireguard`, overridable for tests)
- Produces: launchd daemon `road21-toggle`, `StartInterval = 30`,
  `RunAtLoad = true`, logs to `/var/log/road21-toggle.log`

Behaviour (one run):
1. `gw`/`ifc` from `route -n get default` (`gateway:`, `interface:`). None → exit 0.
2. `mac` from `arp -n "$gw"`. Empty or `(incomplete)` → exit 0.
3. Home (`mac` = `64:62:66:2f:7a:71`) → `wg-quick down` each profile
   that's up; exit.
4. Away, `road21-full` up → `wg-quick down road21` if up; re-resolve
   `road21-full`. Otherwise → `wg-quick up road21` if not up; re-resolve
   `road21`.
5. Re-resolve `p`: `iface` from the `.name` file; `key` from `wg show
   "$iface" peers`; handshake from `wg show "$iface" latest-handshakes`.
   If `now - handshake > 135`: `dns` = `ipconfig getoption "$ifc"
   domain_name_server` (empty → return); `ip` = last IPv4 line of `dig
   +short @"$dns" robot-disco.net A` (empty → return); `wg set "$iface"
   peer "$key" endpoint "$ip:51820"`; for `road21-full` also `route -n
   add -host "$ip" "$gw"` (ignore "File exists").

- [ ] **Step 1: Write the failing desk test**

`tests/road21-toggle-test.sh` puts fakes for `route`, `arp`, `wg`,
`wg-quick`, `ipconfig`, `dig` and `date` first on `PATH`, driven by env
vars (`GW`, `IFC`, `MAC`, `UP` = profiles up, `HANDSHAKE`, `NOW`, `DNS`,
`DIG_IP`). Each fake logs its argv. Run the script with `bash -euo
pipefail` (as `writeShellApplication` does). Table (expected logged
action; `none` = no `wg-quick up/down`, `wg set` or `route add`):

| Case | Inputs | Expected |
|---|---|---|
| home, road21 up | MAC=home, UP=road21 | `wg-quick down road21` |
| home, full up | MAC=home, UP=road21-full | `wg-quick down road21-full` |
| home, none up | MAC=home | none |
| away, none up | MAC=other, fresh handshake | `wg-quick up road21` |
| away, road21 fresh | UP=road21, NOW-HANDSHAKE=10 | none |
| away, both up | UP="road21 road21-full", fresh | `wg-quick down road21` |
| road21 stale | UP=road21, age 200, DIG_IP=203.0.113.7 | `wg set utun9 peer KEY endpoint 203.0.113.7:51820`, no `route add` |
| full stale | UP=road21-full, age 200, DIG_IP=203.0.113.7, GW=10.0.0.1 | that `wg set`, then `route -n add -host 203.0.113.7 10.0.0.1` |
| no default route | GW empty | none, exit 0 |
| arp incomplete | MAC=`(incomplete)` | none, exit 0 |
| no dhcp dns | stale, DNS empty | none, exit 0 |
| dig empty | stale, DIG_IP empty | none, exit 0 |

- [ ] **Step 2: Run it to verify it fails**

Run: `nix shell nixpkgs#gawk -c bash machines/fountain-of-ahmed-iii/tests/road21-toggle-test.sh`
Expected: FAIL (script missing).

- [ ] **Step 3 (owner, optional): Write the home check**

`home_state` prints `home`, `away` or `unknown` (steps 1–3 above). Like
arrakis's `location()`; the owner may write it, Claude reviews with the
harness.

- [ ] **Step 4: Implement the rest of `road21-toggle.sh`**

Constants at the top: `HOME_GW_MAC`, `ENDPOINT_HOST=robot-disco.net`,
`ENDPOINT_PORT=51820`, `STALE_AFTER=135`. Log one line per action.

- [ ] **Step 5: Run test and shellcheck**

Expected: 12/12 PASS; shellcheck silent.

- [ ] **Step 6: Wire the daemon**

`road21Toggle = pkgs.writeShellApplication { name = "road21-toggle";
runtimeInputs = with pkgs; [ wireguard-tools wireguard-go gawk ]; text =
builtins.readFile ./road21-toggle.sh; };` macOS tools (`route`, `arp`,
`ipconfig`, `dig`) come from the inherited launchd `PATH`.
`launchd.daemons.road21-toggle.serviceConfig` per Interfaces.

Check: `nix eval --json .#darwinConfigurations.fountain-of-ahmed-iii.config.launchd.daemons.road21-toggle.serviceConfig --apply 's: { inherit (s) StartInterval RunAtLoad; }'`
→ `{"RunAtLoad":true,"StartInterval":30}`.

- [ ] **Step 7: Commit**

```sh
git add machines/fountain-of-ahmed-iii
nix develop -c git commit -m "feat(fountain): toggle road21 by location, follow DDNS"
```

---

### Task 5: Retire the WireGuard app, go live, verify

**Files:**
- Modify: `machines/fountain-of-ahmed-iii/default.nix` (delete the
  `masApps` block with `"WireGuard" = 1451685025`)

- [ ] **Step 1: Write the failing eval check**

`nix eval .#darwinConfigurations.fountain-of-ahmed-iii.config.homebrew.masApps --apply 'm: m ? WireGuard'`
Expected after: `false`. Now: `true`.

- [ ] **Step 2: Remove the entry; run the check** → `false`.

- [ ] **Step 3 (owner, confirm first): Apply, then clean up by hand**

`just apply` on the Mac. Delete "Home VPN" in System Settings → VPN;
uninstall WireGuard.app. `scutil --nc list` no longer shows
`com.wireguard.macos`.

- [ ] **Step 4 (owner): Live matrix** (`tail -f /var/log/road21-toggle.log`)

| Situation | Expected |
|---|---|
| Home Wi-Fi | no `utun` for road21; Vaultwarden and Seafile load |
| Phone hotspot, Jamf off | `road21` up within ~30 s; both load |
| Hotspot, Jamf on | both still load; `netstat -rn` shows Jamf's routes and `192.168.50.99` via road21's `utun` |
| `sudo wg-quick up road21-full` on hotspot | `road21` goes down; `curl -4 ifconfig.me` = home WAN IP; `curl -6 --max-time 5 ifconfig.me` fails |
| Re-resolve: `sudo wg set $(cat /var/run/wireguard/road21.name) peer <key> endpoint 192.0.2.1:51820` | within ~3 min a fresh handshake; log shows the `wg set` |
| Same with `road21-full` up | repaired too; `route -n get <home WAN IP>` uses the real gateway |
| Back home | everything down; both names load over the intranet |

- [ ] **Step 5: Commit**

```sh
git add machines/fountain-of-ahmed-iii/default.nix
nix develop -c git commit -m "chore(fountain): drop the WireGuard App Store app"
```

---

## Addendum (2026-11-02): where the code ended up

The tasks above are the plan as executed; these are the changes made
afterwards, from the owner's Mac testing and the final branch review. The
code and `tests/` are authoritative.

**Owner fixes from live testing**
- `AbandonProcessGroup` on the `road21-toggle` daemon: launchd killed
  wireguard-go (and with it the utun) when the job exited.
- Dead-interface recovery: a profile whose `.name` file outlives its utun
  is brought down and its `.name` removed, so it reads as down and comes
  back up.
- Home detection: only a real MAC counts as away. `(incomplete)` and the
  `no` from an expired ARP entry (`-- no entry`) read as unknown (Task 4
  behaviour step 2 said only "empty or `(incomplete)`").
- `mac-app-utils` follows nixpkgs-unstable (SBCL heap mapping on macOS 27).

**From the final review**
- Re-resolve under `road21-full`: when the DHCP DNS server is off-link it
  would be reached through the dead tunnel, so it gets a host route via
  the real gateway for the lookup only (test `full stale, off-link dns`).
- Removing the `/etc/hosts` block: set `darktowerNames = [ ]` and switch
  once before deleting the module (test `IP without names removes`).

**Desk tests now:** hosts 7/7, toggle 18/18.

**Still open:** the Task 5 Step 4 live matrix, including a network whose
DHCP DNS server is off-link.
