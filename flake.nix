{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.09";
  inputs.flake-utils.url = "github:numtide/flake-utils/master";

  outputs = { self, nixpkgs, flake-utils, ... }:
    with flake-utils.lib;
    eachSystem [ "x86_64-linux" ] (system:
      with (import nixpkgs { inherit system; });
      let kfk-sender =
            with haskellPackages;
            with haskell.lib;
            overrideCabal (callCabal2nix "kfk-sender" ./kfk-sender {}) (o: {
              librarySystemDepends = [ rdkafka ];
            });
          kfk-reader =
            with haskellPackages;
            with haskell.lib;
            overrideCabal (callCabal2nix "kfk-reader" ./kfk-reader {}) (o: {
              librarySystemDepends = [ rdkafka ];
            });
          qemu-drv = import "${nixpkgs}/nixos" {
            inherit system;
            configuration = { config, pkgs, ...}:
              with pkgs; 
              let username = "authenticator";
                  ghc = haskellPackages.ghcWithPackages (h: [
                    h.hw-kafka-client
                    h.cabal-install
                  ]);
                  cabal-conf = writeText "cabal-conf" ''
                    -- repository hackage.haskell.org
                      -- url: http://hackage.haskell.org/
                      -- secure: True
                      -- root-keys:
                      -- key-threshold: 3
                  '';
                  script = runCommand "kfk-sender-script" {} ''
                    mkdir -pv $out/bin
                    mkdir -pv $out/.cabal
                    cp ${cabal-conf} $out/.cabal/config
                    mkdir -pv $out/dist
                    cd ${kfk-sender-drv.src}
                    echo "${ghc}/bin/cabal --config-file=$out/.cabal/config build --cabal-file=${kfk-sender-drv.src}/kfk-sender.cabal --prefix=$out --builddir=$out/dist --with-compiler=${ghc}/bin/ghc exe:kfk-sender"
                    ${ghc}/bin/cabal --config-file=$out/.cabal/config build --cabal-file=${kfk-sender-drv.src}/kfk-sender.cabal --prefix=$out --builddir=$out/dist --with-compiler=${ghc}/bin/ghc exe:kfk-sender
                  '';
              in {
                networking.firewall.allowedTCPPorts = [ 9092 ];
                
                environment.systemPackages = [
                  rdkafka
                  kfk-sender
                  kfk-reader
                  confluent-platform
                  # script
                ];

                services = {
                  zookeeper.enable = true;
                  apache-kafka = {
                    enable = true;
                    extraProperties = ''
                      offsets.topic.replication.factor=1
                    '';
                  };
                };
                
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
          inherit
            kfk-sender
            kfk-reader
            ;
        };
      });
}
