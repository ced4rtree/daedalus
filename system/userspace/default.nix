{ config, lib, pkgs, ... }: {
  imports = [
    ./bluetooth.nix
    ./brightness.nix
    ./displayManagers
    ./pipewire.nix
    ./printing.nix
    ./shells
    ./stylix.nix
    ./upower.nix
    ./virt-manager.nix
    # ./virtualbox.nix
    ./windowManagers
    ./locale.nix
  ];
}
