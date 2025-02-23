{ emacs, stdenv }:

stdenv.mkDerivation {
  name = "gaelan-emacs-config";
  src = ./init.org;
  nativeBuildInputs = [ emacs ];

  dontUnpack = true;

  buildPhase = ''
    emacs --batch --eval "(require 'org)" \
          --eval "(org-babel-tangle-file \"$src\" \"init.el\" \"emacs-lisp\")"
  '';

  installPhase = ''
    mkdir -p $out/share/emacs/site-lisp
    cp $src $out/share/emacs/site-lisp/init.el
  '';
}
