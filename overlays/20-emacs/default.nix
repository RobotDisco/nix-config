final: prev:

{
  emacs = import ./emacs {
    lib = prev.lib;
    stdenv = prev.stdenv;
  };
}
