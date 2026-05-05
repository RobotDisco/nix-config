{ config, robotdisco-secrets, ... }:

{
  age.secrets = {
    upsmon-primary.file = "${robotdisco-secrets}/ups-user-primary.age";
    upsmon-secondary.file = "${robotdisco-secrets}/ups-user-secondary.age";
  };

  power.ups = {
    enable = true;
    mode = "netserver";
    openFirewall = true;
    ups = {
      ups = {
        driver = "usbhid-ups";
        port = "auto";
      };
    };
    upsd = {
      listen = [
        { address = "127.0.0.1"; }
        # TODO this should be passed in, not hardcoded.
        { address = "192.168.10.3"; }
      ];
    };
    upsmon.monitor.ups = {
      system = "ups@127.0.0.1";
      type = "primary";
      user = "monprime";
      passwordFile = config.age.secrets.upsmon-primary.path;
    };
    users.monuser = {
      passwordFile = config.age.secrets.upsmon-secondary.path;
      upsmon = "secondary";
    };
    users.monprime = {
      passwordFile = config.age.secrets.upsmon-primary.path;
      upsmon = "primary";
    };
  };
}
