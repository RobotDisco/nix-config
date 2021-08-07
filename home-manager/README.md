# Home Manager config

# File structure

## modules/
custom modules. I don't have any yet but I hope to as I understand home-manager more

## profiles/
	This is where end-user config will be stored as with the nixos profiles.

## users/
	This is where individual users will define their configuration, pulling from shared nix files in the profiles/ directory when needed. Very user-specific custom config may also live here if it isn't reusable.
