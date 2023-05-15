# This file follows the callPackage pattern
# see https://nixos.org/guides/nix-pills/callpackage-design-pattern.html
{ lib, packageRequires, stdenv, trivialBuild, version ? "dev" }:

let
  # Nix package for emacs lisp which creates
  # autoloads for all installed emacs packages
  package-quickstart = trivialBuild {
    pname = "config-package-quickstart";
    inherit version packageRequires;

    # We don't need to extract a source archive
    dontUnpack = true;

    # I think the reason we don't need to explicitly reference an emacs
    # package is because the trivialBuild function was itself derived from an
    # emacs package.
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

    # I guess we need go unpack false because we are including source files?
    # dontUnpack = false;

    src = lib.sourceByRegex ./. [ "^init.org$" ];

    buildPhase = ''
      emacs --batch --quick \
        --load org \
        *.org \
        --funcall org-babel-tangle

      # Make a fake config dir because emacs requires it
      mkdir -p .xdg-config
      ln -s $PWD .xdg-config/emacs
      export XDG_CONFIG_HOME="$PWD/.xdg-config"
    '';
  };
in stdenv.mkDerivation {
  pname = "gaelan-emacs-config";
  inherit version;

  # We don't need to extract a source archive
  dontUnpack = true;

  # We pass these through just in case anyone needs the derivations
  # we made to derive our combined and compiled config.
  passthru.components = { inherit package-quickstart init; };

  installPhase = ''
    install -D -t $out ${package-quickstart}/share/emacs/site-lisp/*
    install -D -t $out ${init}/share/emacs/site-lisp/*
  '';
}

