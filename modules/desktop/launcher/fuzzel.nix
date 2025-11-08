{
  flake.modules.homeManager.fuzzel = {
    programs.fuzzel = {
      enable = true;
      settings = {
        main = {
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
  };
}
