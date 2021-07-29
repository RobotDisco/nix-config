#!/usr/bin/env bash

# This mostly follows the standard NixOS installation instuctions and
# the ZFS wisdom found at https://nixos.wiki/wiki/NixOS_on_ZFS
# A few ideas came from my experiences with ZFS on FreeBSD.

# Fail on errors, undefined variables and print all commands
set -uex

# This is a custom version of our installer script which uses
# very specific IDs as we do not want to format the wrong disks
# which will contain irreplacable NAS data

DISK0 = /dev/disk/by-id/ata-WDC_WDS500G2B0A-00SM50_19107F801948
DISK1 = /dev/disk/by-id/ata-WDC_WDS500G2B0A-00SM50_19136D800887

for DSK in ${DISK0} ${DISK1}; do
    parted ${DSK} -- mklabel gpt
    parted ${DSK} -- mkpart ESP fat32 1MB 512MB
    parted ${DSK} -- set 1 boot on

    parted ${DSK} -- mkpart primary 512MB 32GB
    parted ${DSK} -- mkpart primary 32GB -32GB
    parted ${DSK} -- mkpart primary linux-swap -32GB 100%
done

# Format ESP aka boot partition.
# We create partitions on both disks for parity, but we only format and use
# one in actuality
mkfs.fat -F 32 -n EFIBOOT0 ${DISK0}-part1
mkfs.fat -F 32 -n EFIBOOT1 ${DISK1}-part1

# Format root ZFS pool
# fletcher & lz4 are the best balance of speedy/useful checksumming/compression
# xattr and acltype set extended attributres, which systemd/journald needs
# disabling atime is a performance improvement; only /var needs it.
zpool create -O atime=off -O checksum=fletcher4 -O compression=lz4 \
      -O xattr=sa -O acltype=posixacl -O mountpoint=none \
      rootpool mirror ${DISK0}-part2 ${DISK1}-part2

# Create ZFS filesystems
# Anything in rootpool/safe can be backed up if required
# Anything in rootpool/local should be regenerated
zfs create -o mountpoint=none rootpool/local
zfs create -o mountpoint=legacy rootpool/local/nix
zfs create -o mountpoint=none rootpool/safe
zfs create -o mountpoint=none rootpool/safe/ROOT
zfs create -o mountpoint=legacy rootpool/safe/ROOT/nixos
zfs create -o atime=on -o mountpoint=legacy rootpool/safe/var

# Mount filesystems for NixOS installation
mount -t zfs rootpool/safe/ROOT/nixos /mnt

mkdir /mnt/boot /mnt/nix /mnt/var

mount ${DISK0}-part1 /mnt/boot
mount -t zfs rootpool/local/nix /mnt/nix
mount -t zfs rootpool/safe/var /mnt/var

# Format VM ZFS pool
zpool create -O atime=off -O checksum=fletcher4 -O compression=lz4 \
      -O mountpoint=none \
      vmpool mirror ${DISK0}-part3 ${DISK1}-part3

# Format and enable swap partitions.
mkswap -L swap0 ${DISK0}-part4
swapon ${DISK0}-part4
mkswap -L swap1 ${DISK1}-part4
swapon ${DISK1}-part4

# Generate NixOS configuratoin

nixos-generate-config --root /mnt

cp --no-clobber /etc/configuration.nix /mnt/etc/nixos/configuration/nix

echo "Please edit the nix configuration as needed."
echo "$ vi /mnt/etc/nixos/configuratoin.nix"
echo
echo "When ready, install the system and reboot:"
echo "$ nix-install"
echo "$ reboot"
