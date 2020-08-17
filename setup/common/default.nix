{...}:

{
  imports = [
    ./aws.nix
    ./emacs.nix
    ./fonts.nix
    ./google-cloud-platform.nix
    ./packages.nix
  ];

  time.timeZone = "America/Toronto";
}
