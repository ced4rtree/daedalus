{
  flake.modules.homeManager.homestuck = { pkgs, ... }: {
    home.packages = let
      version = "2.7.0";
      homestuck = pkgs.appimageTools.wrapType2 {
        pname = "unofficial-homestuck-collection";
        version = version;
        src = pkgs.fetchurl {
          url = "https://github.com/GiovanH/unofficial-homestuck-collection/releases/download/v${version}/The-Unofficial-Homestuck-Collection-${version}.AppImage";
          hash = "sha256-IfDEgKlRwAlpctiwL+lOTgBdSdUGFxCdArlu+dDiEcY=";
        };
      };
    in with pkgs; [ homestuck ];


    # Get Unofficial Homestuck Collection to show up in application list
    xdg.desktopEntries = {
      unofficial-homestuck-collection = {
        name = "The Unofficial Homestuck Collection";
        exec = "unofficial-homestuck-collection %U";
        icon = "${builtins.fetchurl {
          url = "https://raw.githubusercontent.com/GiovanH/unofficial-homestuck-collection/main/public/icon.png";
          sha256 = "1lvsmdn0km30hnmwnqv81pjj1xhnhf23khb23rpc2f2r3sp3wdq3";
        }}";
        comment = "An offline collection of Homestuck and its related works";
        categories = [ "Game" ];
        startupNotify = false;
        terminal = false;
      };
    };
  };
}
