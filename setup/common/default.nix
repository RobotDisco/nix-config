{...}:

{
  imports = [
    ./aws.nix
    ./google-cloud-platform.nix
    ./packages.nix
  ];

  time.timeZone = "America/Toronto";
}
