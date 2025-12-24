{ config, inputs, ... }: {
  flake.modules.nixos.homestuck = { pkgs, ... }: {
    hj.packages = [ config.flake.packages.${pkgs.stdenv.hostPlatform.system}.unofficial-homestuck-collection ];
  };

  perSystem = { pkgs, lib, system, ... }: let
    version = "2.7.0";
    icon = "${builtins.fetchurl {
      url = "https://raw.githubusercontent.com/GiovanH/unofficial-homestuck-collection/main/public/icon.png";
      sha256 = "1lvsmdn0km30hnmwnqv81pjj1xhnhf23khb23rpc2f2r3sp3wdq3";
    }}";
    homestuckUnwrapped = config.flake.packages.${system}.unofficial-homestuck-collection-unwrapped;
    pname = "unofficial-homestuck-collection";
    pnameUnwrapped = "${pname}-unwrapped";
  in {
    packages.unofficial-homestuck-collection = pkgs.stdenv.mkDerivation rec {
      inherit pname version;
      src = homestuckUnwrapped;

      inherit icon;

      desktopItem = pkgs.makeDesktopItem {
        name = pname;
        desktopName = "The Unofficial Homestuck Collection";
        exec = "${pname} %U";
        comment = "An offline collection of Homestuck and its related works";

        inherit icon;

        categories = [ "Game" ];
        startupNotify = false;
        terminal = false;
      };

      installPhase = ''
        mkdir -p $out/bin
        ln -s ${homestuckUnwrapped}/bin/${pnameUnwrapped} $out/bin/${pname}

        for size in 16 24 32 48 64 128 256 512; do
          mkdir -p $out/share/icons/hicolor/"$size"x"$size"/apps
          ${lib.getExe' pkgs.imagemagick "convert"} \
            -background none \
            -resize "$size"x"$size" \
            ${icon} $out/share/icons/hicolor/"$size"x"$size"/apps/${pname}.png
        done
      '';
    };

    packages.unofficial-homestuck-collection-unwrapped = pkgs.appimageTools.wrapType2 rec {
      pname = pnameUnwrapped;
      inherit version;
      src = pkgs.fetchurl {
        url = "https://github.com/GiovanH/unofficial-homestuck-collection/releases/download/v${version}/The-Unofficial-Homestuck-Collection-${version}.AppImage";
        hash = "sha256-IfDEgKlRwAlpctiwL+lOTgBdSdUGFxCdArlu+dDiEcY=";
      };
    };
  };
}
