{ pkgs, ... }:

{
  # Scanner support
  hardware.sane.enable = true;
  nixpkgs.config.sane.extraFirmware = [{
    src = pkgs.fetchurl {
      # I have a ScanSnap scanner. It has proprietary firmware...
      # http://www.openfusion.net/linux/scansnap_1300i
      url = "http://www.openfusion.net/public/files/1300i_0D12.nal";
      sha256 =
        "cbea48c6cee675c2ea970944b49b805d665ee659f753a50b83c176973f507591";
    };
    name = "1300i_0D12.nal";
    backend = "epjitsu";
  }];
}
