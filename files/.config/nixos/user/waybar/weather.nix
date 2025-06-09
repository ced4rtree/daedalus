{ config, pkgs, lib, ... }: {
  weather-script = pkgs.stdenv.mkDerivation {
    name = "weather.py";
    propagatedBuildInputs = [
      (pkgs.python3.withPackages (pythonPackages: python312Packages; [
        pyquery
      ]))
    ];
    dontUnpack = true;
    installPhase = "install -Dm755 ${./weather.py} $out/bin/weather.py";
  };

  home.packages = [ weather-script ];
}
