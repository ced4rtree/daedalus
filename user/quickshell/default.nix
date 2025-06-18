{ config, lib, pkgs, inputs, ... }: {
  home.packages = let
    beat_detector = pkgs.stdenv.mkDerivation {
      pname = "beat_detector";
      version = "1.0.0";

      src = ./beat_detector.cpp;

      # buildInputs = with pkgs; [
      #   threads
      # ];

      nativeBuildInputs = with pkgs; [
        pipewire aubio
      ];

      buildPhase = ''
        g++ -std=c++17 -O3 -Wall -Wextra -I/usr/include/pipewire-0.3 \
          -I/usr/include/spa-0.2 -I/usr/include/aubio -o beat_detector \
          beat_detector.cpp -lpipewire-0.3 -laubio -pthread
      '';

      installPhase = ''
        mkdir -p $out/bin
        cp beat_detector $out/bin
      '';
    };
  in [
    inputs.quickshell.packages."x86_64-linux".default
    pkgs.kdePackages.qtdeclarative
    pkgs.material-symbols
    pkgs.cava
    pkgs.lm_sensors
    pkgs.python312Packages.gpustat
    beat_detector
  ];

  home.file.".config/quickshell".source = ./files;
}
