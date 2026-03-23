{ pkgs }:
{
  emacs = pkgs.callPackage ./emacs { };
}
// (pkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
  sunsama = pkgs.callPackage ./sunsama.nix { };
})
