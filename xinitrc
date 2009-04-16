#!/bin/sh

numlockx on
display -window root -backdrop ~/.xmonad/bg
xscreensaver -no-splash &
urxvtd -q -f -o
parcellite -d &
exec xmonad
