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
    version = "4.9.0";
    sourceRoot = "Slack.app";
    src = super.fetchurl {
      url = "https://downloads.slack-edge.com/releases/macos/${version}/prod/x64/Slack-${version}-macOS.dmg";
      sha256 = "007fflncvvclj4agb6g5hc5k9j5hhz1rpvlcfd8w31rn1vad4abk";
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
    version = "81.0";
    sourceRoot = "Firefox.app";
    src = super.fetchurl {
      url = "https://download-installer.cdn.mozilla.net/pub/firefox/releases/${version}/mac/en-CA/Firefox%20${version}.dmg";
      sha256 = "ea3357277d236208980940850f0728eff9ad5fa8f2e8c40d5fc8af79df24c554";
      name = "Firefox-${version}.dmg";
    };
    description = "The browser that respects your privacy";
    homepage = https://mozilla.org;
  };

  Deezer = self.installApplication rec {
    name = "Deezer";
    version = "4.23.1";
    sourceRoot = "Deezer.app";
    src = super.fetchurl {
      url = "http://cdn-content.deezer.com/builds/deezer-desktop/8cF2rAuKxLcU1oMDmCYm8Uiqe19Ql0HTySLssdzLkQ9ZWHuDTp2JBtQOvdrFzWPA/darwin/x64/${version}/DeezerDesktop_${version}.dmg";
      sha256 = "f7520c05a2f3f113ab44ca3cb2cde1dbe3070152ba93fabafea42dd8389e9954";
    };
    description = "A world of music in your pocket.";
    homepage = https://deezer.com;
  };

  SeafileClient = self.installApplication rec {
    name = "SeafileClient";
    version = "7.0.9";
    sourceRoot = "Seafile\ Client.app";
    src = super.fetchurl {
      url = "https://s3.eu-central-1.amazonaws.com/download.seadrive.org/seafile-client-${version}.dmg";
      sha256 = "b00e859fc55d05fe0af1dfaddab64d166875e90b5969289fd35b4811aaeea71c";
    };
    description = "Seafile is an open source enterprise file sync and share platform with high reliability and performance.";
    homepage = "https://seafile.com";
  };

  Signal = self.installApplication rec {
    name = "Signal";
    version = "1.36.3";
    sourceRoot = "Signal.app";
    src = super.fetchurl {
      url = "https://updates.signal.org/desktop/signal-desktop-mac-${version}.dmg";
      sha256 = "94ffa7b4615d783290066dc5f7dd4dd956d1fe229f334dee543627979846daac";
    };
    description = "State-of-the-art end-to-end encryption";
    homepage = "https://www.signal.org/";
  };

  Kobo = self.installApplication rec {
    name = "Kobo";
    version = "0.0.1";
    sourceRoot = "Kobo.app";
    src = super.fetchurl {
      url = "https://kbdownload1-a.akamaihd.net/desktop/kobodesktop/kobosetup.dmg";
      sha256 = "23368860e3e9ec726538251cfb5589fb57a73ff2f4c13678246921c3f9a23e95";
    };
    description = "kobo.com eBooks and Audiobooks";
    homepage = "https://www.kobo.com";
  };

  Steam = self.installApplication rec {
    name = "Steam";
    version = "0.0.1";
    sourceRoot = "Steam.app";
    src = super.fetchurl {
      url = "https://cdn.cloudflare.steamstatic.com/client/installer/steam.dmg";
      sha256 = "dda4744327fe200e08d132ccbba9828b6bde8672080a69f69d52e72e9a6bda17";
    };
    description = "Steam is the ultimate destination for playing, discussing, and creating games.";
    homepage = "https://steampowered.com";
  };

  Discord = self.installApplication rec {
    name = "Discord";
    version = "0.0.259";
    sourceRoot = "Discord.app";
    src = super.fetchurl {
      url = "https://dl.discordapp.net/apps/osx/${version}/Discord.dmg";
      sha256 = "927b7174671aeede430e5a71f9a8eee1991e2dab5309a8ee623dd1fb04706517";
    };
    description = "Step up your game with a modern voice & text chat app.";
    homepage = "https://discord.com";
  };
}
