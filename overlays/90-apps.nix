self: super: {

installApplication = 
  { name, appname ? name, version, src, description, homepage, 
    postInstall ? "", sourceRoot ? ".", ... }:
  with super; stdenv.mkDerivation {
    name = "${name}-${version}";
    version = "${version}";
    src = src;
    buildInputs = [ undmg unzip ];
    sourceRoot = sourceRoot;
    phases = [ "unpackPhase" "installPhase" ];
    installPhase = ''
      mkdir -p "$out/Applications/${appname}.app"
      cp -pR * "$out/Applications/${appname}.app"
    '' + postInstall;
    meta = with stdenv.lib; {
      description = description;
      homepage = homepage;
      platforms = platforms.darwin;
    };
  };

Calibre = self.installApplication rec {
  name = "Calibre";
  version = "4.22.0";
  sourceRoot = "Calibre.app";
  src = super.fetchurl {
    url = "https://download.calibre-ebook.com/${version}/calibre-${version}.dmg";
    sha256 = "82fe3c269e4a126f9a7cc59dd24413b69f2448c125870072275634441c0e4073";
    # date = 2018-03-10T23:36:13-0700;
  };
  description = "Calibre is a one stop solution for all your ebook needs.";
  homepage = https://calibre-ebook.com;
  # appcast = https://github.com/kovidgoyal/calibre/releases.atom;
};

Slack = self.installApplication rec {
  name = "Slack";
  version = "4.8.0";
  sourceRoot = "Slack.app";
  src = super.fetchurl {
    url = "https://downloads.slack-edge.com/releases/macos/${version}/prod/x64/Slack-${version}-macOS.dmg";
    sha256 = "428ec2b5a9d5eb3b408c1cafa3977daeff0391c7d71656773c6ce535d8e0424c";
  };
  description = "Slack is the collaboration hub that brings the right people, information, and tools together to get work done.";
  homepage = https://slack.com;
};

Bitwarden = self.installApplication rec {
  name = "Bitwarden";
  version = "1.20.1";
  sourceRoot = "Bitwarden.app";
  src = super.fetchurl {
    url = "https://github.com/bitwarden/desktop/releases/download/v${version}/Bitwarden-${version}.dmg";
    sha256 = "c8bc79c18e7c163d8bd2410f875b9427bd3e5c52357720e05fc49c7729be04fa";
  };
  description = "Bitwarden helps you generate, save and manage your passwords safely and securely.";
  homepage = https://bitwarden.com;
};

Firefox = self.installApplication rec {
  name = "Firefox";
  version = "79.0";
  sourceRoot = "Firefox.app";
  src = super.fetchurl {
    url = "https://download-installer.cdn.mozilla.net/pub/firefox/releases/${version}/mac/en-CA/Firefox%20${version}.dmg";
    sha256 = "bd5b12cdcb25c516490b6563178f5acc4064047c4341c5126dcf180c0ab02eb4";
    name = "Firefox-${version}.dmg";
  };
  description = "The browser that respects your privacy";
  homepage = https://mozilla.org;
};

}
