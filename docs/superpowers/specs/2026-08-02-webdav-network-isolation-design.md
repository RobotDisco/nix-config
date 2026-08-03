# Webdav (Seafile) Network Isolation — Design

**Date:** 2026-08-02
**Status:** Draft for review
**Author:** Gaelan D'costa (with Claude)

## 1. Problem & goal

`machines/darktower/services/webdav.nix` runs Seafile (`seafile-mc` +
`seafile-memcached`) as `virtualisation.oci-containers` (podman) on the
host's **default podman network**, published via
`ports = [ "192.168.10.3:8001:80" ]`. `192.168.10.3` is not an arbitrary
address — it's `eno1` in `machines/darktower/network.nix`, explicitly
commented there as the **admin/control-plane interface** ("don't route
external ingress traffic through here as a form of network security").
`reverseproxy` (on `br50`, the internet-facing segment) proxies
`fallcube.robot-disco.net` directly to that address.

This means an internet-facing service is bound, by construction, onto
the same interface as host administration and the Unifi network
controller — the opposite of the isolation this repo already achieves
for `vaultwarden`, `reverseproxy`, and `feeds` (each an nspawn container
on its own bridge/subnet, `privateNetwork = true`).

**Goal:** move Seafile off the admin interface and onto the same kind of
isolated nspawn container the other internet-facing services already
use, without rewriting Seafile itself (no native `services.seafile`
module exists in nixpkgs — confirmed absent from
`nixos/modules/module-list.nix` — which is why this repo runs the
vendor `seafileltd/seafile-mc` Docker image via podman in the first
place).

## 2. Scope

**In scope**

- Wrap the existing podman workload (`seafile-memcached`, `seafile-mc`)
  inside a new NixOS nspawn container (`containers.webdav`), nesting
  podman inside it — same image, same volumes, same secrets, different
  network placement.
- Move that container onto `br50` (the existing internet-facing
  segment shared with `reverseproxy`/`vaultwarden`/`feeds`), not a new
  VLAN.
- Update `reverseproxy`'s `proxyPass` for `fallcube.robot-disco.net` to
  the new address.
- Update `mysql.nix`'s firewall rule, since seafile's path to MySQL
  changes from the host's `podman0` interface to `br50`.
- A pilot step to de-risk nested podman-in-nspawn before touching real
  data.

**Out of scope (separate, previously identified issues)**

- Giving `postgresql`/`mysql` their own `privateNetwork` isolation
  (they currently share the host's network namespace). Real gap, not
  this change's job.
- Re-enabling or changing `orgnote.nix` (confirmed not imported by
  `machines/darktower/default.nix` — dead code, intentionally left as
  -is).
- Fixing `freshrss.nix` (untracked, currently has unbalanced braces —
  unrelated work in progress).

## 3. Key decisions (settled during brainstorming)

1. **Nest podman inside an nspawn container, don't rewrite Seafile** —
   nspawn (`containers.<name>`) and podman/OCI are different layers
   (system-config virtualization vs. process/image virtualization);
   nesting is a normal, supported composition. Rewriting Seafile as
   native NixOS services would mean reverse-engineering the vendor
   image's internal supervision of ccnet/seahub/seaf-server with no
   upstream module to lean on — high effort, real data risk, not
   justified when nesting achieves the actual goal (network isolation).
2. **Reuse `br50`, don't create a new VLAN/bridge** — segment by
   trust/blast-radius tier, not by app. `br50` already hosts services of
   comparable sensitivity (`reverseproxy`, `vaultwarden`, `feeds`); each
   is still bounded by its own per-container firewall, so sharing the
   bridge doesn't widen exposure beyond the ports each already exposes
   to the internet. A new VLAN would need physical switch trunk changes
   for a marginal isolation gain (mostly against L2-level attacks), and
   would not be proportionate at this scale. The boundary that actually
   matters — internet-facing apps vs. the admin plane — is preserved by
   getting Seafile onto *any* isolated nspawn container; which `br50`
   address it gets is comparatively low-stakes.
3. **MySQL firewall rule widens from `podman0` to `br50`, offset by a
   host-scoped MySQL grant** — once seafile-mc is nested, its traffic to
   MySQL arrives via `br50`, not the host's `podman0`. The interface
   rule alone would allow any `br50` peer, not just the new container;
   to keep the same per-service scoping Postgres already has via
   `pg_hba` (restricted to `192.168.50.2/32`), add a MySQL grant scoped
   to the new container's specific address rather than relying on the
   firewall alone.
4. **Pilot nested podman-in-nspawn before migrating real data** — this
   repo has no existing example of podman running inside an nspawn
   container, and NixOS nspawn containers need specific
   capability/cgroup delegation for podman to function at all inside
   them (unverified on this host). Validate with a throwaway container
   first; only then touch the real `webdav.nix` and its live data
   volume.

## 4. Architecture

```
Before:
  internet -> reverseproxy (br50/.99)
           -> host podman0 / eno1 (192.168.10.3:8001)   <-- admin interface
           -> seafile-mc + seafile-memcached
           -> mysql (host netns, via podman0)

After:
  internet -> reverseproxy (br50/.99)
           -> containers.webdav (br50/.4)  <-- new isolated nspawn container
                -> nested podman0 (private to this container's netns)
                     -> seafile-mc + seafile-memcached (unchanged internally)
           -> mysql (host netns, via br50 now, scoped grant to .4)
```

The only thing that changes is the network path. Seafile's image,
volumes, environment, and database backend are untouched.

## 5. Components / config changes

- **`webdav.nix`** — restructured so the existing
  `virtualisation.oci-containers.containers` block (`seafile-memcached`,
  `seafile-mc`, unchanged apart from the port-publish address) lives
  inside `containers.webdav.config`, alongside
  `virtualisation.podman.enable = true`. The container gets
  `privateNetwork = true`, `hostBridge = "br50"`,
  `localAddress = "192.168.50.4/24"`, and a `bindMounts` entry
  forwarding the host's `/srv/storagepool/data/webdav/shared` into the
  container (which podman then mounts again into `seafile-mc`, same
  as vaultwarden's two-layer bind-mount pattern).
- **`reverseproxy.nix`'s `fallcube.robot-disco.net` vhost** (currently
  declared in `webdav.nix`) — `proxyPass` moves from
  `http://192.168.10.3:8001` to `http://192.168.50.4:80` (or whichever
  port the nested podman publishes on).
- **`mysql.nix`** — firewall rule changes from
  `networking.firewall.interfaces.podman0.allowedTCPPorts = [ 3306 ]` to
  `interfaces.br50.allowedTCPPorts = [ 3306 ]`, plus a MySQL grant
  scoped to `192.168.50.4` for the seafile database user(s), mirroring
  Postgres's per-source-IP scoping.
- **agenix (`age.secrets.seafile-envs`)** — needs verification at
  implementation time: whether it can be declared inside
  `containers.webdav.config` directly (per-container agenix) or whether
  the container needs the host's already-decrypted secret bind-mounted
  read-only instead.

## 6. Data flow

No change to Seafile's data model, DB backend, or secrets — only the
network hop changes (see §4 diagram). The bind-mounted data directory
is never moved or copied; it's mounted read-write into the new
container exactly as it's mounted into the current host-level podman
setup.

## 7. Rollout

1. **Pilot (must pass before step 2):** stand up a throwaway nspawn
   container with only `virtualisation.podman.enable = true`, confirm a
   trivial `podman run` succeeds inside it on this host/kernel. This is
   the one genuinely uncertain technical piece; validate it cheaply
   before editing the real config.
2. Write the real `webdav.nix` migration (§5) as a new `containers.webdav`
   block, initially **alongside** the existing host-level
   `oci-containers` definition (both can't run at once on the real data
   volume — the pilot in step 1 is what gets tested live; the real
   container is brought up as a genuine cutover, not run in parallel
   against the same bind mount).
3. Bring up the new container, verify Seafile is reachable internally
   at `192.168.50.4` (curl from the host or reverseproxy container),
   confirm MySQL connectivity over `br50`.
4. Flip `reverseproxy`'s `proxyPass` to the new address, confirm
   externally via `fallcube.robot-disco.net`.
5. Remove the old host-level `oci-containers` block and the old
   `podman0` MySQL firewall rule.

## 8. Rollback

The bind-mounted data directory is untouched throughout — there is no
data-migration step to reverse. Rollback is `git revert` of the cutover
commit(s) + `just test-switch`, restoring the prior host-level podman
configuration.

## 9. Testing / validation

- `just check` / `just build-all` for eval correctness.
- `just test-switch` (non-destructive) before `just apply`.
- Manual verification after cutover:
  - Seafile web UI loads and login/file-sync works via
    `fallcube.robot-disco.net`.
  - MySQL connectivity confirmed from `192.168.50.4`.
  - **The actual point of this change:** confirm port 8001 on
    `eno1`/`192.168.10.3` no longer listens once the old block is
    removed.

## 10. Risks / open questions

- **Nested podman-in-nspawn capability requirements** — not yet
  verified on this host (cgroup delegation, overlayfs mounts,
  capabilities). Addressed by the mandatory pilot in §7 step 1; if the
  pilot fails, fall back options (e.g. `vfs` storage driver instead of
  `overlay`, or additional `containers.webdav.additionalCapabilities`)
  need to be explored before proceeding.
- **agenix per-container secrets** — unconfirmed whether
  `age.secrets` can be declared inside a nspawn container's own config
  scope or needs a host-decrypted bind-mount. Resolve during
  implementation (§5).
- **MySQL grant scoping** — the exact mechanism for restricting a
  MySQL user to a specific source host (`CREATE USER 'x'@'192.168.50.4'`
  equivalent in whatever ensureUsers/ensureDatabases pattern
  `services.mysql` supports) needs confirming against the NixOS mysql
  module's actual options; not yet verified against nixpkgs.
