{
  appimageTools,
  fetchurl,
  lib,
}:
let
  pname = "sunsama";
  version = "3.0.8-build-250227nscg4ey0r";

  # Sunsama's URL is annoying and requires unknown implicit magic to getch the
  # appropriate binary for the appropriate platform.
  # Homebrew got around this by finding the electron->native generator they use.
  # I'm not sure how they reverse engineered this but I will use it.
  #
  # To find recent versions, use the following urls
  # https://download.todesktop.com/2003096gmmnl0g1/latest-mac.yml
  # https://download.todesktop.com/2003096gmmnl0g1/latest-linux.yml
  src = fetchurl {
    url = "https://download.todesktop.com/2003096gmmnl0g1/sunsama-${version}-x86_64.AppImage";
    hash = "sha512-0wFdaGXZNbffFDcxROuMh28rFWt+qF9OG+FT1ZdyT7khQoLVt+79IDGudQmQLbLNHeRd+iN2Sw1fuoBm8z9DQA==";
  };

  appimageContents = appimageTools.extract { inherit pname version src; };
in
appimageTools.wrapType2 {
  inherit pname version src;

  extraInstallCommands = ''
    # mv $out/bin/${pname}-${version} $out/bin/${pname}
    install -m 444 -D ${appimageContents}/sunsama.desktop $out/share/applications/sunsama.desktop
    install -m 444 -D ${appimageContents}/usr/share/icons/hicolor/1024x1024/apps/sunsama.png $out/share/icons/hicolor/1024x1024/apps/sunsama.png
    substituteInPlace $out/share/applications/sunsama.desktop --replace 'Exec=AppRun' 'Exec=${pname}'
  '';

  meta = with lib; {
    homepage = "https://sunsama.com";
    description = "The digital daily planner that helps you feel calm and stay focused.";
    platforms = [ "x86_64-linux" ];
    license = licenses.unfree;
    maintainers = [
      {
        name = "Gaelan D'costa";
        email = "gdcosta@gmail.com";
      }
    ];
  };
}
