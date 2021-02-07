# TODO somehow create /var/lib/nut

{
  power.ups = {
    enable = true;
    mode = "netserver";
    ups = {
      ups = {
        driver = "usbhid-ups";
        port = "auto";
      };
    };
  };

  environment.etc."nut/upsd.conf" = {
    text = "
LISTEN 127.0.0.1
#LISTEN 192.168.60.2
";
    mode = "600";
  };

  # So you'd think these should be secret, and you'd be right, but I'm forced
  # to use the values from synology so these are effectively public anyway.
  # This does mean I should make sure this doesn't get listend to from
  # public networks.
  environment.etc."nut/upsd.users" = {
    text = "
[monuser]
    password = secret
    upsmon slave
[monmaster]
    password = secret
    usbmon master
";
    mode = "600";
  };

  environment.etc."nut/upsmon.conf" = {
    text = "
MONITOR ups@127.0.0.1 1 monmaster secret master
SHUTDOWNCMD /run/current-system/sw/bin/poweroff
";
    mode = "600";
  };
}
