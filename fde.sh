#!/usr/bin/env sh

# All instructions taken from https://nixos.wiki/wiki/Yubikey_based_Full_Disk_Encryption_(FDE)_on_NixOS

rbtohex() {
    ( od -An -vtx1 | tr -d ' \n' )
}

hextorb() {
    ( tr '[:lower:]' '[:upper:]' | sed -e 's/\([0-9A-F]\{2\}\)/\\\\\\x\1/gI'| xargs printf )
}

nix-env -i gcc-wrapper
nix-env -i yubikey-personalization
nix-env -i openssl

cc -O3 \
   -I$(nix-build "<nixpkgs>" --no-build-output -A openssl.dev)/include \
   -L$(nix-build "<nixpkgs>" --no-build-output -A openssl.out)/lib \
   $(nix eval "(with import <nixpkgs> {}; pkgs.path)")/nixos/modules/system/boot/pbkdf2-sha512.c \
   -o ./pbkdf2-sha512 -lcrypto
chmod +x ./pbkdf2-sha512

SLOT=2
# Don't need to do this after the first time
ykpersonalize -"$SLOT" -ochal-resp -ochal-hmac

SALT_LENGTH=16
salt="$(dd if=/dev/random bs=1 count=$SALT_LENGTH 2>/dev/null | rbtohex)"

echo "Enter a passphrase: "
read -s k_user

challenge="$(echo -n $salt | openssl dgst -binary -sha512 | rbtohex)"
response="$(ykchalresp -2 -x $challenge 2>/dev/null)"

KEY_LENGTH=512
ITERATIONS=1000000

k_luks="$(echo -n $k_user | ./pbkdf2-sha512 $(($KEY_LENGTH / 8)) $ITERATIONS $response | rbtohex)"

EFI_PART=/dev/nvmen0p1
LUKS_PART=/dev/nvmen0p2

EFI_MNT=/root/boot
mkdir "$EFI_MNT"
mkfs.vfat -F 32 -n EFIBOOT0 "$EFI_PART"
mount "$EFI_PART" "$EFI_MNT"

STORAGE=/crypt-storage/default
mkdir -p "$(dirname $EFI_MNT$STORAGE)"

echo -ne "$salt\n$ITERATIONS" > $EFI_MNT$STORAGE

CIPHER=aes-xts-plain64
HASH=sha512
echo -n "$k_luks" | hextorb | cryptsetup luksFormat --cipher="$CIPHER" \ 
  --key-size="$KEY_LENGTH" --hash="$HASH" --key-file=- "$LUKS_PART"


LUKSROOT=nixos-enc
echo -n "$k_luks" | hextorb | cryptsetup luksOpen $LUKS_PART $LUKSROOT --key-file=-

pvcreate "/dev/mapper/$LUKSROOT"

VGNAME=partitions
vgcreate "$VGNAME" "/dev/mapper/$LUKSROOT"

lvcreate -L 64G -n swap "$VGNAME"
FSROOT=rootpart0
lvcreate -L 16G -n "$FSROOT" "$VGNAME"
FSNIX=nixpart0
lvcreate -L 16G -n "$FSNIX" "$VGNAME"
FSVAR=varpart0
lvcreate -L 16G -n "$FSVAR" "$VGNAME"
FSHOME=homepart0
lvcreate -l 100%FREE -n "$FSHOME" "$VGNAME"

vgchange -ay

mkswap -L swappart0 /dev/partitions/swap

mkfs.ext4 -L "$FSROOT" "/dev/partitions/$FSROOT"
mkfs.ext4 -L "$FSNIX" "/dev/partitions/$FSNIX"
mkfs.ext4 -L "$FSVAR" "/dev/partitions/$FSVAR"
mkfs.ext4 -L "$FSHOME" "/dev/partitions/$FSHOME"

mount -o noatime "/dev/partitions/$FSROOT" /mnt
mkdir /mnt/nix
mount -o noatime "/dev/partitions/$FSNIX" /mnt/nix
mkdir /mnt/var
mount -o relatime "/dev/partitions/$FSVAR" /mnt/var
mkdir /mnt/home
mount -o noatime "/dev/partitions/$FSHOME" /mnt/home
mkdir /mnt/boot
mount "/dev/partitions/$EFI_PART" /mnt/boot

swapon /dev/partitions/swap