{pkgs, ...}:

{
  imports = [
    ../common
  ];

  environment.systemPackages = with pkgs; [
    Bitwarden
    # Calibre # Uses an APFS dmg which isn't supported by undmg
    Deezer
    Firefox
    SeafileClient
    Slack
  ];
}
