{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/master";

  outputs = { self, nixpkgs }: {
    packages.x86_64-linux = {
      vm = (import "${nixpkgs}/nixos" {
        system = "x86_64-linux";
        configuration = { config, pkgs, ...}:
          let username = "authenticator";
              ghc = with pkgs; haskell.packages.ghcHEAD.ghcWithPackages (h: [
                h.hw-kafka-client
              ]);
              
          in {
            networking.firewall.allowedTCPPorts = [ 3000 ];
            
            environment.systemPackages = [];
            
            users = {
              mutableUsers = false;
              users = {
                root.password = "";
                "${username}".isSystemUser = true;
              };
            };

            virtualisation = {
              graphics = false;
              memorySize = 2048;
            };
          };
      }).vm;
    };
  };
}
