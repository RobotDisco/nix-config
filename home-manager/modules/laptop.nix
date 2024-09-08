{ lib, ... }:

{
  options.robot-disco.laptop.bluetoothID = lib.mkOption {
    type = lib.types.int;
    description = "Bluetooth device ID to toggle via rfkill";
  };
}
