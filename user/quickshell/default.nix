{ config, lib, pkgs, inputs, ... }: {
  home.packages = [
    inputs.quickshell.packages."x86_64-linux".default
    pkgs.kdePackages.qtdeclarative
    pkgs.material-symbols
    pkgs.cava
    pkgs.lm_sensors
  ];

  home.file.".config/quickshell".source = ./files;
}
