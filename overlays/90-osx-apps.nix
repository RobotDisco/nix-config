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
    version = "5.14.0";
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
    version = "4.17.0";
    sourceRoot = "Slack.app";
    src = super.fetchurl {
      url = "https://downloads.slack-edge.com/releases/macos/${version}/prod/x64/Slack-${version}-macOS.dmg";
      sha256 = "0r5cafxw73qnn14ljprn7w8bfn67zbkcniq60k9pf2zbqgb4cyj9";
    };
    description = "Slack is the collaboration hub that brings the right people, information, and tools together to get work done.";
    homepage = https://slack.com;
  };

  Bitwarden = self.installApplication rec {
    name = "Bitwarden";
    version = "1.26.5";
    sourceRoot = "Bitwarden.app";
    src = super.fetchurl {
      url = "https://github.com/bitwarden/desktop/releases/download/v${version}/Bitwarden-${version}.dmg";
      sha256 = "0nv905mvxvzgnsa2g913fj8bcxbpz6s1hhl44l368bqc5sz0kdjq";
    };
    description = "Bitwarden helps you generate, save and manage your passwords safely and securely.";
    homepage = https://bitwarden.com;
  };

  Firefox = self.installApplication rec {
    name = "Firefox";
    version = "89.0";
    sourceRoot = "Firefox.app";
    src = super.fetchurl {
      url = "https://download-installer.cdn.mozilla.net/pub/firefox/releases/${version}/mac/en-CA/Firefox%20${version}.dmg";
      sha256 = "1p83c1q3v6czba7612lk57rrilwyclv3jd9837qwv09wahmlcxm7";
      name = "Firefox-${version}.dmg";
    };
    description = "The browser that respects your privacy";
    homepage = https://mozilla.org;
  };

  Deezer = self.installApplication rec {
    name = "Deezer";
    version = "4.32.0";
    sourceRoot = "Deezer.app";
    src = super.fetchurl {
      url = "http://cdn-content.deezer.com/builds/deezer-desktop/8cF2rAuKxLcU1oMDmCYm8Uiqe19Ql0HTySLssdzLkQ9ZWHuDTp2JBtQOvdrFzWPA/darwin/x64/${version}/DeezerDesktop_${version}.dmg";
      sha256 = "1wslgi2q4m3qj7vpqp2pzd0bh2zbgw5npid5578nyvn9qycgwq4k";
    };
    description = "A world of music in your pocket.";
    homepage = https://deezer.com;
  };

  SeafileClient = self.installApplication rec {
    name = "SeafileClient";
    version = "8.0.2";
    sourceRoot = "Seafile\ Client.app";
    src = super.fetchurl {
      url = "https://s3.eu-central-1.amazonaws.com/download.seadrive.org/seafile-client-${version}.dmg";
      sha256 = "19arxbms1d3apj2il0i83s53iflzap6c1fsxhskwja3bwha3znzv";
    };
    description = "Seafile is an open source enterprise file sync and share platform with high reliability and performance.";
    homepage = "https://seafile.com";
  };

  Signal = self.installApplication rec {
    name = "Signal";
    version = "5.5.0";
    sourceRoot = "Signal.app";
    src = super.fetchurl {
      url = "https://updates.signal.org/desktop/signal-desktop-mac-${version}.dmg";
      sha256 = "0sm5x31a8b9w9scgj5dybc0ri6y31za4zv884sg3px52ngf76yhl";
    };
    description = "State-of-the-art end-to-end encryption";
    homepage = "https://www.signal.org/";
  };

  Kobo = self.installApplication rec {
    name = "Kobo";
    version = "4.24.11789";
    sourceRoot = "Kobo.app";
    src = super.fetchurl {
      url = "https://kbdownload1-a.akamaihd.net/desktop/kobodesktop/kobosetup.dmg";
      sha256 = "f61bce74a1eebbe11c8b4d709c2b1266c36d288b056620e68628b39fb5f4bb37";
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
    version = "0.0.262";
    sourceRoot = "Discord.app";
    src = super.fetchurl {
      url = "https://dl.discordapp.net/apps/osx/${version}/Discord.dmg";
      sha256 = "1afb0qj23hmhsz9jgzwaj5gqsxdwdxi8bcc656s27qhydnbxbgpn";
    };
    description = "Step up your game with a modern voice & text chat app.";
    homepage = "https://discord.com";
  };

  Remarkable = self.installApplication rec {
    name = "Remarkable";
    version = "2.8.0.166";
    sourceRoot = "Remarkable.app";
    src = super.fetchurl {
      url = "https://downloads.remarkable.com/desktop/production/mac/reMarkable-${version}.dmg";
      sha256 = "1n4gl55sp4gxcl7sjj0aj7dhxs0if93yk61w1yk6yb0b9p59sd4q";
    };
    description = "The next-generation paper tablet.";
    homepage = "https://remarkable.com";
  };

  Tidal = self.installApplication rec {
    name = "TIDAL";
    version = "2.27.6.234";
    sourceRoot = "TIDAL.app";
    src = super.fetchurl {
      url = "https://download.tidal.com/desktop/TIDAL.dmg";
      sha256 = "0v5s6bwna2jhks4721fn1x3f6yhzgwbn5skimj8rgd4jp85i2h56";
    };
    description = "70+ million songs. Ad-free.";
    homepage = "https://tidal.com";
  };
}
