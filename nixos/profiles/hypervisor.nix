{ pkgs, ... }:

{
  users.users.gaelan = {
    extraGroups = [ "libvirtd" ];
  };

  boot.kernelParams = [ "kvm-intel" ];
  
  virtualisation.libvirtd = {
    enable = true;
    # We only intend to be runnning
    # accelerated virtualisation, not
    # alien architectures.
    qemuPackage = pkgs.qemu_kvm;
  };
}
