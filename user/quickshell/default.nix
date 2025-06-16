{ config, lib, pkgs, inputs, ... }: {
  home.packages = [
    inputs.quickshell.packages."x86_64-linux".default
    pkgs.kdePackages.qtdeclarative
    pkgs.material-symbols
  ];

  home.file.".config/quickshell" = ./files;
}
