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
    ] ++ lib.attrVals (import ./_commonModules.nix) config.flake.modules.nixos;

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
