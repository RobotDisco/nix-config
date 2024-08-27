{ config, ... }:

{
  # Ensure that the kernel installed supports ZFS
  boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;

  # Support ZFS filesystems
  boot.supportedFilesystems = [ "zfs" ];

  # Always enable trim support
  services.zfs.trim.enable = true;

  # Use interleaved schedule of bimonthly scrubs
  # and long SMART tests (and weekly short SMART tests)
  # found at https://www.truenas.com/community/threads/scrub-and-smart-testing-schedules.20108/
  # Scrub ZFS pools every bimonthly
  services.zfs.autoScrub = {
    interval = "*-*-01,15 03:00";
    enable = true;
  };

  # Email if something goes wrong.
  # services.zfs.zed = {
  #   enableMail = true;
  #   settings = {
  #     ZED_EMAIL_ADDR = [ "gdcosta@gmail.com" ];
  #   };
  # };
}
