{ config, lib, ... }: {
  flake.modules.nixos.base = { pkgs, ... }: {
    imports = with config.flake.modules.nixos; [
      grub
      sudo
      nix
      networkManager
      timeZone
      kernel
      hostOptions
      sops
      stylix
      users
    ];

    environment.systemPackages = with pkgs; [
      neovim
      git
      gnugrep
      ripgrep
      wget
      openssh
      unzip
    ];
  };
}
