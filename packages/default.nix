{ pkgs }:
{
  emacs = pkgs.callPackage ./emacs { };
  mujmap = pkgs.callPackage ./mujmap.nix { };
}
// (pkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
  sunsama = pkgs.callPackage ./sunsama.nix { };
})
