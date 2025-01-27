{ config, pkgs, ... }: {
  home.packages = let
    homestuck = pkgs.appimageTools.wrapType2 {
      name = "unofficial-homestuck-collection";
      version = "2.5.1";
      src = pkgs.fetchurl {
        url = "https://github.com/GiovanH/unofficial-homestuck-collection/releases/download/v2.5.1/The-Unofficial-Homestuck-Collection-2.5.1.AppImage";
        hash = "sha256-K93bd+Zbt/HKzNz78EjJpLQy+H0UOuQ7tvWPN6LLJd4=";
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
}
