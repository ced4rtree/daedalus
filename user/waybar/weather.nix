{ config, pkgs, lib, ... }: {
  home.packages = let
    weather-script = pkgs.stdenv.mkDerivation {
      name = "weather.py";
      propagatedBuildInputs = [
        (pkgs.python3.withPackages (pythonPackages: with pkgs.python312Packages; [
          pyquery
        ]))
      ];
      dontUnpack = true;
      installPhase = "install -Dm755 ${./weather.py} $out/bin/weather.py";
    };
  in
    [ weather-script ];
}
