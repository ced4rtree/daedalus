{ config, lib, pkgs, ... }: {
  imports = [
    ./bluetooth.nix
    ./brightness.nix
    ./displayManagers
    ./pipewire.nix
    ./printing.nix
    ./shells
    ./upower.nix
    ./virt-manager.nix
    # ./virtualbox.nix
    ./windowManagers
    ./locale.nix
  ];
}
