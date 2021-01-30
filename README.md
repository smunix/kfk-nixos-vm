# kfk-nixos-vm
NixOS based Kafka vm with Haskell kafka reader and sender

Start the VM with (assuming your nix supports flakes): 

```sh
nix shell github:smunix/kfk-nixos-vm#qemu -c run-nixos-vm
```

Once inside the Qemu VM, start the consumer with `kfk-reader`, then spawn your producer with `kfk-sender`
