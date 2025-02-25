{
  emacs,
  lib,
  stdenv,
}:

stdenv.mkDerivation {
  name = "gaelan-emacs-config";
  src = lib.sourceByRegex ./. [ "^init.org$" ];
  nativeBuildInputs = [ emacs ];

  dontUnpack = true;

  buildPhase = ''
    cp $src/*.org .
    emacs --quick --batch --load org \
          *.org --funcall org-babel-tangle 
  '';

  installPhase = ''
    install -D -m 644 -t $out *.el
  '';
}
