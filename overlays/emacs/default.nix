final: _prev:

{
  gaelan-emacs = final.callPackage ./runtime.nix {
    baseEmacs = final.emacs-pgtk;
  };
  gaelan-emacs-macport = final.callPackage ./runtime.nix {
    baseEmacs = final.emacs-macport;
  };
  gaelan-emacs-config = final.callPackage ./config.nix { };
}
