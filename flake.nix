{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.03";

  outputs = { self, nixpkgs }: {
    packages.x86_64-linux = {
      vm = (import "${nixpkgs}/nixos" {
        system = "x86_64-linux";
        configuration = { config, pkgs, ...}:
          let username = "authenticator";
          in {
            networking.firewall.allowedTCPPorts = [ 3000 ];

            users = {
              mutableUsers = false;
              users = {
                root.password = "";
                "${username}".isSystemUser = true;
              };
            };
          };
      }).vm;
    };
  };
}
