# NixOS Machine modules

This is where machine definitions for NixOS machines will be set up.

These will contain machine-specific configuration, and link to more general collections of configuration profiles in the <kbd>nixos/profiles</kbd> directory.

## File organization 

For most straightforward configurations, these will likely contain two files:

### hardware-configuration.nix

This is what the NixOS autogenerates as detected boot hardware. Don't modify this so we can safely regenerate it.

### configuration.nix

This is where we define user configuration. It likely should contain

* a hostname definition
* a state version definition (so we can upgrade state config deliberately)
* a list of imports
* any misc machine-specific config that isn't common enough to be its own profile
