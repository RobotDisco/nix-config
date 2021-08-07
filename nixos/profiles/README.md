# NixOS Profiles

## File organization

It is a half-baked intention that nixos machine definitions should only need to load some combination of profiles from the root profiles/ directory, everything in subdirectories are subcomponents that will always be included as part of a root profile.
