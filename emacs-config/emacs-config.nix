# This file follows the callPackage pattern, whatever that means
{ version ? "dev", packageRequires, lib, stdenv, trivialBuild }:

let
  # Package for emacs lisp which creates
  # autoloads for all installed packages
  package-quickstart = trivialBuild {
    pname = "config-package-quickstart";
    inherit version packageRequires;

    dontUnpack = true;

    buildPhase = ''
      emacs --batch --quick \
        --load package \
        --eval '(setq package-quickstart-file "package-quickstart.el")' \
        --eval '(setq package-quickstart t)' \
        --funcall package-quickstart-refresh
    '';
  };

  # Generate .el from org files, and compile them
  init = trivialBuild {
    pname = "config-init";
    inherit version packageRequires;

    src = lib.sourceByRegex ./. [ "init.org" ];

    buildPhase = ''
      emacs --batch --quick \
        --load org \
        *.org \
        --funcall org-babel-tangle

      mkdir -p .xdg-config
      ln -s $PWD .xdg-config/emacs
      export XDG_CONFIG_HOME="$PWD/.xdg-config"

      emacs -L . --batch --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile *.el
    '';
  };
in
stdenv.mkDerivation {
  pname = "emacs-config";
  inherit version;

  dontUnpack = true;

  passthru.components = {
    inherit package-quickstart init;
  };

  installPhase = ''
    install -D -t $out ${package-quickstart}/share/emacs/site-lisp/*
    install -D -t $out ${init}/share/emacs/site-lisp/*
  '';
}
  
 
