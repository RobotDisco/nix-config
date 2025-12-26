{
  containers.reverseproxy.config.services.nginx.virtualHosts."brain.robot-disco.net" = {
    extraConfig = ''
      proxy_set_header X-Forwarded-For $remote_addr;
    '';

    locations."/" = {
      proxyPass = "http://192.168.10.3:8002";

      extraConfig = ''
        client_max_body_size 0;
      '';
    };

    forceSSL = true;
    enableACME = true;
  };

  virtualisation.oci-containers.containers = {
    orgnote_mongo = {
      autoStart = true;
      image = "mongo:5.0.9";
      environment = {
        MONGO_INITDB_ROOT_USERNAME = "orgnote";
        MONGO_INITDB_ROOT_PASSWORD = "password";
      };
      volumes = [ "/srv/storagepool/data/orgnote/db:/data/db" ];
      cmd = [ "--auth" ];
    };
    orgnote_backend = {
      autoStart = false;
      dependsOn = [ "orgnote_mongo" ];
      image = "orgnote/backend";
      environment = {
        APP_ADDRESS = "0.0.0.0:3000";
        DEBUG = "false";
        MONGO_USERNAME = "orgnote";
        MONGO_PASSWORD = "password";
        MONGO_URL = "orgnote_mongo";
        MONGO_PORT = "27017";
        GITHUB_ID = "Ov23licg9Jx9QxqZURBg";
        GITHUB_SECRET = "26185bd436983fa6448a20defd6cf4e257a75d64";
        CLIENT_ADDRESS = "https://brain.robot-disco.net";
        BACKEND_HOST = "brain.robot-disco.net/api";
        BACKEND_DOMAIN = "brain.robot-disco.net/api";
        BACKEND_SCHEMA = "https";
        ACCESS_CHECK_URL = "";
        ACCESS_CHECK_TOKEN = "";
        MAXIMUM_FILE_SIZE = "2147483647";
      };
      volumes = [ "/srv/storagepool/data/orgnote/media:/workspace/media" ];
    };
    orgnote_cors = {
      autoStart = true;
      image = "bassetts/warp-cors";
    };
    orgnote_client = {
      autoStart = true;
      image = "orgnote/client";
      environment = {
        DISABLE_LOGGER = "1";
      };
    };
    orgnote_nginx = {
      autoStart = true;
      image = "nginx:stable";
      ports = [ "192.168.10.3:8002:80" ];
      volumes = [
        "/home/gaelan/orgnote-client/dist:/opt/services/frontend:rw"
        "/home/gaelan/orgnote-backend/deploy/nginx/conf.d:/etc/nginx/conf.d:rw"
        "/srv/storagepool/data/orgnote/media:/opt/services/backend/media:rw"
        "/home/gaelan/orgnote-backend/static:/opt/services/backend/static:rw"
      ];
      dependsOn = [
        "orgnote_backend"
        "orgnote_cors"
        "orgnote_client"
      ];
      entrypoint = "/usr/sbin/nginx";
      cmd = [
        "-g"
        "daemon off;"
      ];
    };
  };
}
