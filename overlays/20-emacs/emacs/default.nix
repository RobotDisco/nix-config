{ lib
, stdenv
}:

let
  pkgs = import <nixpkgs> {
    overlays =  [(import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))];
  };

  emacsEnv = pkgs.emacsWithPackagesFromUsePackage {
    config = ./init.org;
  };

  init = emacsEnv.trivialBuild {
    pname = "config-init";

    src = lib.sourceByRegex ./. [ "init.org" ];

    preBuild = ''
      # Tangle org files
      emacs --batch -Q \
        -l org \
        *.org \
        -f org-babel-tangle

      # Fake config directory in order to have files on load-path
      mkdir -p .xdg-config
      ln -s $PWD .xdg-config/emacs
      export XDG_CONFIG_HOME="$PWD/.xdg-config"

      emacs --batch -Q \
        -l package \
        -eval '(setq package-quickstart t)' \
        -f package-quickstart-refresh
    '';
  };
in
stdenv.mkDerivation {
  pname = "emacs-config";

  dontUnpack = true;

  buildInputs = [ pkgs.emacs-all-the-icons-fonts pkgs.python3Minimal pkgs.ripgrep ];

  installPhase = ''
    install -D -t $out ${init}/share/emacs/site-lisp/*
  '';
}
