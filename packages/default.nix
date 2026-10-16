{ pkgs }:
{
  emacs = pkgs.callPackage ./emacs { };
  mujmap = pkgs.callPackage ./mujmap.nix { };
}
// (pkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
  sunsama = pkgs.callPackage ./sunsama.nix { };
})
// (pkgs.lib.optionalAttrs (pkgs.stdenv.hostPlatform.system == "aarch64-linux") {
  # Code-kata Emacs for the uConsole. aarch64-linux only.
  emacs-kata = pkgs.callPackage ./emacs-kata { };
})
