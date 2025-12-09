{ config, ... }: {
  flake.modules.nixos.alexandria = { lib, pkgs, modulesPath, ... }: {
    imports = with config.flake.modules.nixos; [
      (modulesPath + "/profiles/qemu-guest.nix")
      server
    ];

    daedalus.efi.enable = false;

    boot.initrd.availableKernelModules = [ "ahci" "xhci_pci" "virtio_pci" "sr_mod" "virtio_blk" ];
    boot.initrd.kernelModules = [ ];
    boot.kernelModules = [ "kvm-intel" ];
    boot.extraModulePackages = [ ];

    fileSystems."/" =
      { device = "/dev/disk/by-uuid/d2aceeca-cfd0-4b92-9358-1abb61aae0c4";
        fsType = "ext4";
      };

    swapDevices = [ ];

    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  };
}
