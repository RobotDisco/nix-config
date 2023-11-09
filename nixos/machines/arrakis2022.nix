{ pkgs, ... }:

let
  displays = {
    builtin = {
      device = "eDP-1";
      fingerprint = "00ffffffffffff0009e55f0900000000171d0104a51c137803de50a3544c99260f505400000001010101010101010101010101010101115cd01881e02d50302036001dbe1000001aa749d01881e02d50302036001dbe1000001a000000fe00424f452043510a202020202020000000fe004e4531333546424d2d4e34310a00fb";
    };
    landscape = {
      device = "DP-1-1-1";
      fingerprint = "00ffffffffffff0010ac7ba04c31543035150104a53420783aee95a3544c99260f5054a1080081408180a940b300d1c0010101010101283c80a070b023403020360006442100001a000000ff004d324743523143533054314c0a000000fc0044454c4c2055323431324d0a20000000fd00323d1e5311000a2020202020200096";
    };
    portrait = {
      device = "DP-2-2-1";
      fingerprint = "00ffffffffffff0010ac7ba0535347300f1a0104a53420783aee95a3544c99260f5054a1080081408180a940b300d1c0010101010101283c80a070b023403020360006442100001a000000ff004854354e33363446304753530a000000fc0044454c4c2055323431324d0a20000000fd00323d1e5311000a202020202020009f";
    };
  };

in

{
  imports = [
    ../profiles/audio.nix
    ../profiles/boot.nix
    ../profiles/firmware.nix
    ../profiles/hidpi.nix
    ../profiles/home-devices.nix
    ../profiles/laptop.nix
    ../profiles/nix.nix
    ../profiles/regional.nix
    ../profiles/security.nix
    ../profiles/steam.nix
    ../profiles/virtualbox.nix
    ../profiles/window-manager.nix
    ../profiles/yubikey.nix
    ../../users/gaelan
  ];

  config = {
    boot.initrd.availableKernelModules =
      [ "xhci_pci" "thunderbolt" "nvme" "usb_storage" "usbhid" "sd_mod" ];
    boot.kernelModules = [ "dm-snapshot" ];

    networking.hostName = "arrakis";

    # Enable the Framework networking device
    networking.interfaces.wlp170s0.useDHCP = true;

    # Support thunderbolt
    services.hardware.bolt.enable = true;

    # Framework firmware is in the lvfs-testing repo
    services.fwupd.extraRemotes = [
      "lvfs-testing"
    ];

    services.autorandr = {
      enable = true;

      profiles = {
        default = {
          fingerprint."${displays.builtin.device}" = displays.builtin.fingerprint;
          config = {
            "${displays.builtin.device}" = {
              enable = true;
              mode = "2256x1504";
              position = "0x0";
            };
          };
        };
        clamshell-open = {
          fingerprint = {
            "${displays.builtin.device}" = displays.builtin.fingerprint;
            "${displays.landscape.device}" = displays.landscape.fingerprint;
            "${displays.portrait.device}" = displays.portrait.fingerprint;
          };
          config = {
            "${displays.builtin.device}" = {
              enable = true;
              mode = "2256x1504";
              position = "0x1200";
            };
            "${displays.landscape.device}" = {
              enable = true;
              primary = true;
              mode = "1920x1200";
              position = "0x0";
              scale.x = 1;
              scale.y = 1;
            };
            "${displays.portrait.device}" = {
              enable = true;
              mode = "1920x1200";
              position = "1920x0";
              scale.x = 1;
              scale.y = 1;
              rotate = "right";
            };
          };
        };
        clamshell-closed = {
          fingerprint = {
            "${displays.landscape.device}" = displays.landscape.fingerprint;
            "${displays.portrait.device}" = displays.portrait.fingerprint;
          };
          config = {
            "${displays.landscape.device}" = {
              enable = true;
              primary = true;
              mode = "1920x1200";
              position = "0x0";
              scale.x = 1;
              scale.y = 1;
            };
            "${displays.portrait.device}" = {
              enable = true;
              mode = "1920x1200";
              position = "1920x0";
              scale.x = 1;
              scale.y = 1;
              rotate = "right";
            };
          };
        };
      };
    };
  };
}
