{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/master";
  inputs.flake-utils.url = "github:numtide/flake-utils/master";

  outputs = { self, nixpkgs, flake-utils, ... }:
    with flake-utils.lib;
    eachSystem [ "x86_64-linux" ] (system:
      with (import nixpkgs { inherit system; });
      let kfk-sender-drv =
            with haskellPackages;
            with haskell.lib;
            overrideCabal (callCabal2nix "kfk-sender" ./kfk-sender {}) (o: {
              librarySystemDepends = [ rdkafka ];
            });
          qemu-drv = import "${nixpkgs}/nixos" {
            system = "x86_64-linux";
            configuration = { config, pkgs, ...}:
              with pkgs; 
              let username = "authenticator";
                  ghc = haskellPackages.ghcWithPackages (h: [
                    h.hw-kafka-client
                    h.cabal-install
                  ]);
                  script = runCommand "haskell-script" {} ''
                    mkdir -pv $out/bin
                    cd ${kfk-sender-drv.src}
                    ls -lh .
                  '';
              in {
                networking.firewall.allowedTCPPorts = [ 9092 ];
                
                environment.systemPackages = [
                  cabal-install
                  rdkafka
                  kfk-sender-drv
                  script
                ];
                
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
          };
      in {
        packages = flattenTree {
          qemu = qemu-drv.vm;
          kfk-sender = kfk-sender-drv;
        };
      });
}
