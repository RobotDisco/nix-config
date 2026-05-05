{
  config,
  pkgs,
  robotdisco-secrets,
  ...
}:

let
  upsTarget = "ups@127.0.0.1";
  upsUser = "gaelan";

  mkUpsCmd =
    name: cmd: desc:
    pkgs.writeShellScriptBin name ''
      # ${desc}
      exec ${pkgs.nut}/bin/upscmd \
        -u ${upsUser} \
        -p "$(cat ${config.age.secrets.upsmon-gaelan.path})" \
        ${upsTarget} ${cmd} "$@"
    '';
in
{
  age.secrets = {
    upsmon-primary.file = "${robotdisco-secrets}/ups-user-primary.age";
    upsmon-secondary.file = "${robotdisco-secrets}/ups-user-secondary.age";
    upsmon-gaelan.file = "${robotdisco-secrets}/ups-user-test.age";
    # Separate mount owned by gaelan so the battery-test scripts can read it.
    upsmon-gaelan.owner = "gaelan";
  };

  environment.systemPackages = [
    (mkUpsCmd "ups-test-quick" "test.battery.start.quick" "Run a quick (~10s) UPS self-test")
    (mkUpsCmd "ups-test-deep" "test.battery.start.deep"
      "Run a deep UPS battery test (full discharge/recharge cycle)"
    )
    (mkUpsCmd "ups-test-stop" "test.battery.stop" "Stop an in-progress UPS battery test")
  ];

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
    users = {
      gaelan = {
        passwordFile = config.age.secrets.upsmon-gaelan.path;
        instcmds = [
          "test.battery.start.quick"
          "test.battery.start.deep"
          "test.battery.stop"
        ];
      };
      monuser = {
        passwordFile = config.age.secrets.upsmon-secondary.path;
        upsmon = "secondary";
      };
      monprime = {
        passwordFile = config.age.secrets.upsmon-primary.path;
        upsmon = "primary";
      };
    };
  };
}
