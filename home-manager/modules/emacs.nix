{
  config,
  pkgs,
  emacsTarot,
  robotdisco-secrets,
  ...
}:

let
  emacsPackage = pkgs.callPackage ../../packages/emacs { inherit emacsTarot; };
  aspellWithDicts = pkgs.aspellWithDicts (
    dicts: with dicts; [
      en
      en-science
      en-computers
    ]
  );
  emacsConfig =
    pkgs.runCommand "emacs-config"
      {
        nativeBuildInputs = [ pkgs.emacs-nox ];
      }
      ''
        mkdir -p $out
        cp ${../../packages/emacs/init.org} init.org
        emacs --batch --load org init.org --funcall org-babel-tangle
        substituteInPlace init.el \
          --replace-fail "@gpgconf@" "${pkgs.gnupg}"
        cp *.el $out/
      '';
in
{
  services.emacs = {
    enable = true;
    package = emacsPackage;
    defaultEditor = true;
    socketActivation.enable = true;
  };

  home = {
    packages = with pkgs; [
      emacsPackage
      # Dictionary support for emacs spellchecking
      aspellWithDicts
      # org-roam graph support
      graphviz
      # Image displaying/modification support
      imagemagick
      # LaTeX support
      texlive.combined.scheme-full
      # ePub support
      unzip
      # Typeface
      nerd-fonts.anonymice
    ];
    shellAliases = {
      erecovers = "find ~/Documents/brain -name '#*#' -print";
    };
  };

  programs.git.settings = {
    diff.tool = "ediff";
    difftool.diff.cmd = ''
      $EDITOR --eval '(ediff-files "'$LOCAL'" "'$REMOTE'")'
    '';

    mergetool.ediff.cmd = ''
      $EDITOR --eval '(ediff-merge-files-with-ancestor "'$LOCAL'" "'$REMOTE'" '"$BASE'" nil "'$MERGED'")'
    '';
  };

  age.secrets = {
    emacs-authinfo = {
      file = "${robotdisco-secrets}/emacs-authinfo.age";
      path = "${config.home.homeDirectory}/.authinfo";
    };
    emacs-xoauth2-el = {
      file = "${robotdisco-secrets}/emacs-xoauth2-el.age";
      path = "${config.xdg.configHome}/emacs/xoauth2.el";
    };
  };

  xdg = {
    enable = true;
    configFile."emacs/init.el".source = "${emacsConfig}/init.el";
    configFile."emacs/early-init.el".source = "${emacsConfig}/early-init.el";
  };
}
