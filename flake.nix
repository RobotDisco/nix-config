{
  description = "Gaelan's nix-based systems configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, nixos-hardware, home-manager }: {
    nixosConfigurations = {
      arrakis = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          nixpkgs.nixosModules.notDetected
          nixos-hardware.nixosModules.framework
          home-manager.nixosModules.home-manager
          ./nixos/machines/arrakis2022.nix
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.gaelan = {
              # Set up some reasonble and secure ssh configuration
              programs.ssh = {
                enable = true;
                compression = true;
                # Don't forward by default, it is insecure
                # Prefer proxyjumping if you can
                forwardAgent = false;
              };

              # install and configure git
              programs.git = {
                enable = true;
                extraConfig = {
                  core = { autocrlf = "input"; };
                  hub = { protocol = "https"; };
                };

                # TODO write a tulip nix-shell that sets these to tulip email addresses.
                userEmail = "gdcosta@gmail.com";
                userName = "Gaelan D'costa";
              };

              programs.gpg = {
                enable = true;
                settings = {
                  personal-cipher-preferences = "AES256 AES192 AES";
                  personal-digest-preferences = "SHA512 SHA384 SHA256";
                  personal-compress-preferences = "ZLIB BZIP2 ZIP Uncompressed";
                  default-preference-list =
                    "SHA512 SHA384 SHA256 AES256 AES192 AES ZLIB BZIP2 ZIP Uncompressed";
                  cert-digest-algo = "SHA512";
                  s2k-digest-algo = "SHA512";
                  s2k-cipher-algo = "AES256";
                  charset = "utf-8";
                  fixed-list-mode = true;
                  no-comments = true;
                  no-emit-version = true;
                  no-greeting = true;
                  keyid-format = "0xlong";
                  list-options = "show-uid-validity";
                  verify-options = "show-uid-validity";
                  with-fingerprint = true;
                  require-cross-certification = true;
                  no-symkey-cache = true;
                  use-agent = true;
                  throw-keyids = true;
                };
              };

              services.gpg-agent = {
                enable = true;
                enableExtraSocket = true;
                enableSshSupport = true;
                extraConfig = ''
                  allow-emacs-pinentry
                  allow-loopback-pinentry
                '';
                defaultCacheTtl = 60;
                maxCacheTtl = 120;
              };
            };
          }
        ];
      };
    };
  };
}
