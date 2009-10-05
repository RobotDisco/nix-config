#!/usr/bin/zsh

# Sanitize keyboard
xmodmap -e "remove Lock = Caps_Lock"
xmodmap -e "keysym Caps_Lock = Control_L"
xmodmap -e "add Control = Control_L"

numlockx on
display -window root -backdrop ~/.xmonad/bg

# Run essential applications
xscreensaver -no-splash &
urxvtd -q -f -o &
parcellite -dn &

if [ -d "$HOME/.cabal/bin" ]; then
	PATH=$PATH:$HOME/.cabal/bin
fi

exec xmonad
