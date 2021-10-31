{
  sops.secrets.seafile_env_vars = {
    sopsFile = ../../secrets/seafile.yaml;
    format = "yaml";
  };

  # Since we're using bridge mode, we need to allow access to our DB container
  networking.firewall.interfaces."br-561697bea5b0".allowedTCPPorts = [ 3306 ];

  virtualisation.oci-containers.containers = {
    "seafile-memcached" = {
      autoStart = true;
      image = "memcached:1.5.6";
      entrypoint = "memcached";
      cmd = ["-m" "256"];
      extraOptions = [
        "--network=seafile"
      ];
    };
    "seafile" = {
      autoStart = true;
      image = "seafileltd/seafile-mc:8.0.7";
      ports = [ "8001:80" ];
      dependsOn = [ "seafile-memcached"];
      environmentFiles = [ "/run/secrets/seafile_env_vars" ];
      volumes = [
        "/srv/data/webdav/shared:/shared"
      ];
      extraOptions = [
        "--network=seafile"
        "--add-host=host.docker.internal:host-gateway"
      ];
    };
  };
}
