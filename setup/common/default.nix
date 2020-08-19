{...}:

{
  imports = [
    ./aws.nix
    ./emacs.nix
    ./fonts.nix
    ./gnupg.nix
    ./google-cloud-platform.nix
    ./packages.nix
  ];

  time.timeZone = "America/Toronto";
}
