{ config, ... }:

let
  hostKey = "/etc/ssh/ssh_host_ed25519_key";
in
{
  # Track trunk on GitHub so darktower converges on whatever was last pushed.
  # The lockfile is used as-is (no --update-input): CI bumps flake.lock and
  # builds/caches darktower, so the server only ever builds a tested lock.
  system.autoUpgrade = {
    enable = true;
    flake = "github:RobotDisco/nix-config#${config.networking.hostName}";
    dates = "04:00";
    randomizedDelaySec = "45min";
    # Kernel/ZFS updates are staged for the next boot; reboot manually so a
    # file server never goes down unattended.
    allowReboot = false;
  };

  # The robotdisco-secrets input is fetched over git+ssh. Authenticate as the
  # host key, which is registered as a read-only deploy key on nix-secrets.
  systemd.services.nixos-upgrade.environment.GIT_SSH_COMMAND =
    "ssh -i ${hostKey} -o IdentitiesOnly=yes";

  programs.ssh.knownHosts."github.com".publicKey =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl";
}
