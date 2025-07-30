{ config, lib, pkgs, ... }: {
  programs.fuzzel = {
    enable = true;
    settings = {
      main = {
        font = "JetBrainsMono Nerd Font:size=10";
        fields = "filename,name,generic,keywords,comment,categories";
        terminal = "footclient -e";
        anchor = "center";
        width = 75;
      };

      border = {
        width = 2;
        radius = 0;
      };
    };
  };
}
