{pkgs, ...}:

{
  imports = [
    ../common
    ./emacs.nix
  ];

  environment.systemPackages = with pkgs; [
    Bitwarden
    # Calibre # Uses an APFS dmg which isn't supported by undmg
    Deezer
    Discord
    Firefox
    Kobo
    Remarkable
    SeafileClient
    Signal
    Slack
    Steam
  ];
}
