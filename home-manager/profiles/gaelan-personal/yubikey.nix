{ pkgs, ... }:

{
  home.packages = [
    pkgs.yubioath-flutter
  ];
  
  pam.yubico.authorizedYubiKeys.ids = [
    # Main Yubikey
    "cccccclbhcbn"
    # Backup Yubikey
    "cccccclbhcbc"
  ];
}
