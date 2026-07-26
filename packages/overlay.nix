# Local overlay composed by the system builders
# (lib/{nixos,darwin}System.nix) and the homeConfigurations entries
# in flake.nix, so every place that imports nixpkgs sees the same
# custom packages. Use this only for packages that need to be
# globally available; per-consumer overrides (e.g. mujmap via
# `programs.mujmap.package`) belong at the consumer site instead.
# Use `prev` (not `final`) for the platform gate: the *shape* of the
# overlay (which attrs it defines) must be determinable without
# referring to `final`, otherwise nixpkgs cycles trying to compute
# `final` from this overlay's output. Values can still use `final`
# since they're forced lazily.
final: prev:
{
  orgnote-cli = final.callPackage ./orgnote.nix { };
}
// prev.lib.optionalAttrs prev.stdenv.isLinux {
  sunsama = final.callPackage ./sunsama.nix { };
}
