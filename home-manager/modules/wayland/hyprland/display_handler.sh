#!/usr/bin/env sh

lid_state=$(awk '{ print $2 }' < /proc/acpi/button/lid/LID0/state)

if [ "$lid_state" = "open" ]; then
    hyprctl keyword monitor "eDP-1, preferred, auto, 1.566667";
    hyprctl keyword monitor "desc:Dell Inc. DELL U2412M M2GCR1CS0T1L,disable";
    hyprctl keyword monitor "desc:Dell Inc. DELL U2412M HT5N364F0GSS,disable";
else
    hyprctl keyword monitor "desc:Dell Inc. DELL U2412M M2GCR1CS0T1L, preferred, 0x0, auto";
    hyprctl keyword monitor "desc:Dell Inc. DELL U2412M HT5N364F0GSS, preferred, auto-right, auto, transform, 3";
    hyprctl keyword monitor "eDP-1, disable";
fi
