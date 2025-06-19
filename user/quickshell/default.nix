{ config, lib, pkgs, inputs, ... }: {
  home.packages = let
  in [
    inputs.quickshell.packages."x86_64-linux".default
    pkgs.kdePackages.qtdeclarative
    pkgs.material-symbols
    pkgs.cava
    pkgs.lm_sensors
    pkgs.python312Packages.gpustat
  ];

  home.file.".config/quickshell".source = ./files;
}
